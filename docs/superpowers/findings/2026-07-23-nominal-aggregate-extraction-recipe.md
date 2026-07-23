# Nominal final-demand / import aggregate: extraction recipe from raw NATIODOMIMP

Ticket #10 (wayfinder, second stage). READ-ONLY investigation; no R executed.

## Question

How does Stage A extract the nominal final-demand and import aggregate from the raw
`inputs/NATIODOMIMP/*dom.csv` tables, precisely enough to recompute the nominal GDP
aggregate `Σ_j (1−m_j)·F_j = Ft_total − M_total` INDEPENDENTLY, without running Stage A?

Record: which rows/columns, which transforms, currency convention, and any ambiguity
that blocks an exact reconstruction.

## Raw table format (`*dom.csv`)

Physical layout, from `ARG1996dom.csv` (representative; all 1976 files share the schema)
and the metadata in `etc/Modelo/IOTs_ReadMe.xlsx`:

- **Units / currency**: "Data are expressed in current million USD"
  (`etc/Modelo/IOTs_ReadMe.xlsx`, shared-strings). The raw tables are NOMINAL, current-price,
  in **USD millions**. They are NOT in national currency and NOT deflated. This is the OECD
  ICIO "national IOT" (NIOT), domestic/import-split variant.
- **Dimensions on disk**: 96 lines × 56 fields. Line 1 is the header; column 1 is an
  (empty-named) row-label column. `read.csv()` therefore yields a data.frame of **95 data rows
  × 55 data columns**, with the row labels in column 1 and numeric data in columns 2–56.
- **Column layout** (header, `ARG1996dom.csv:1`):
  - Cols 2–46: **45 detailed sectors**, ISIC-Rev4 codes `A01_02 … T`.
  - Cols 47–56: **10 final-demand / balancing columns** in this fixed order:
    `HFCE, NPISH, GGFC, GFCF, INVNT, DPABR, CONS_NONRES, EXPO, IMPO, TOTAL`.
    - HFCE = household final consumption; NPISH = non-profits serving households;
      GGFC = government final consumption; GFCF = gross fixed capital formation;
      INVNT = changes in inventories; DPABR = direct purchases abroad by residents;
      CONS_NONRES = direct purchases by non-residents (an export); EXPO = cross-border exports;
      IMPO = cross-border imports (a subtracted/negative column); TOTAL = row total.
- **Row layout** (data.frame row index i = CSV line i+1; verified by `awk` label dump):
  - df rows 1–45: `DOM_*` — domestic-origin flows, 45 sectors (`ARG1996dom.csv:2–46`).
  - df rows 46–90: `IMP_*` — imported-origin flows, 45 sectors (`ARG1996dom.csv:47–91`).
  - df row 91: `TXS_IMP_FNL` — taxes less subsidies on products paid in foreign countries
    (import taxes) (`ARG1996dom.csv:92`).
  - df row 92: `TXS_INT_FNL` — taxes less subsidies on products paid to domestic agencies
    (`ARG1996dom.csv:93`).
  - df row 93: `TTL_INT_FNL` — total intermediate consumption at purchasers' prices
    (`ARG1996dom.csv:94`). NOT used by Stage A.
  - df row 94: `VALU` — value added at basic prices (`ARG1996dom.csv:95`).
  - df row 95: `OUTPUT` — output at basic prices, i.e. gross output (`ARG1996dom.csv:96`).
- **DOM_ / IMP_ meaning**: each intermediate/final flow is split by the origin of the good.
  `DOM_` = supplied by domestic production; `IMP_` = supplied by imports. The sum
  (DOM_ + IMP_) equals the corresponding total-use (NATIOTTL) cell.
- **One file = one country-year**: filename `<ISO3><YEAR>dom.csv` (e.g. `ARG1996dom.csv`).
  Country codes are the leading 3 chars (`etc/Modelo/base/base.R:28`); years span 1995–2020
  (`etc/Modelo/compilacao.R:13`). TWN (Taiwan) is excluded (`etc/Modelo/base/base.R:25`).

## Extraction recipe (Stage-A reference)

Driver order: `etc/Modelo/compilacao.R:18–25`
(titulos → deflator → exchange_rate → base → matrizes → coeficientes_tecnicos → matriz_impacto).
`n_setores = 7` (`etc/Modelo/compilacao.R:14`).

### Shared machinery

- **`titulo`** = 45 detailed sector codes; **`agregador`** = a 45×7 0/1 membership matrix
  read from `etc/Modelo/agregador.xlsx` (`etc/Modelo/base/titulos.R:26–34`). The 7 aggregate
  sectors are: Agropecuaria, Commodities Agricolas, Commodities Industriais, Industria
  Tradicional, Industria Inovativa, Servicos, Infraestrutura (verified by dumping the xlsx).
- **Scalar multiplier `deflator`** (badly named): `deflator_geral$Value = exchange_rate / deflator_GDP`
  (`etc/Modelo/base/base.R:5–10`), where `exchange_rate` = national currency per USD
  (`etc/Modelo/base/exchange_rate.R:1`) and `deflator_GDP` = GDP deflator index
  (`etc/Modelo/base/deflator.R`). Selected per country-year at
  `etc/Modelo/base/matrizes.R:19–24` and applied as `Meepo * deflator`.
  Net effect on every extracted block: `raw_USD × exchange_rate / deflator_GDP`
  = **constant-price national currency**. Extraction is per country over all years
  (`etc/Modelo/base/matrizes.R:5–33`).

### Block extraction (`Un.R`, `Um.R`, `Ut.R`, `Recurso.R`)

- **CIn** (domestic intermediate matrix, 7×7): raw block `[1:45, 2:46]`, row-named `titulo`,
  aggregated `t(agregador) %*% M %*% agregador`, then `× deflator`
  (`etc/Modelo/base/Un.R:3–17`).
- **Fn** (domestic final demand, 7×5): raw block `[1:45, 47:56]`, aggregated on rows only
  `t(agregador) %*% M`, then column-adjusted: `HFCE = HFCE + NPISH`,
  `EXPO = EXPO + CONS_NONRES`, and drop `NPISH, CONS_NONRES, DPABR, IMPO, TOTAL`
  (`etc/Modelo/base/Un.R:20–35`), then `× deflator` (`:38`). Result columns:
  `HFCE, GGFC, GFCF, INVNT, EXPO` (5 components). Domestic Fn **drops DPABR** (residents'
  purchases abroad are not domestic output).
- **CIm** (imported intermediate matrix, 7×7): raw block `[46:90, 2:46]`, same aggregation and
  `× deflator` (`etc/Modelo/base/Um.R:3–17`).
- **Fm** (imported final demand, 7×5): raw block `[46:90, 47:56]`, row-aggregated, then
  `HFCE = HFCE + NPISH + DPABR`, `EXPO = EXPO + CONS_NONRES`, drop
  `NPISH, CONS_NONRES, DPABR, IMPO, TOTAL` (`etc/Modelo/base/Um.R:20–35`), `× deflator` (`:38`).
  Imported Fm **adds DPABR into HFCE** (direct purchases abroad are imports of consumption).
- **CI = CIn + CIm; Ft = Fn + Fm** (`etc/Modelo/base/Ut.R:4,13`). Ut applies NO further
  multiplier (already applied to the components).
- **Recurso** (supply side): raw rows `c(91,92,94,95)` = `TXS_IMP_FNL, TXS_INT_FNL, VALU, OUTPUT`,
  columns `2:46`, aggregated on columns only `M %*% agregador`; then rows combined into
  `II = TXS_IMP_FNL + TXS_INT_FNL`, `VA = VALU`, `GO = OUTPUT`, `× deflator`
  (`etc/Modelo/base/Recurso.R:4–25`). `GO` (= raw `OUTPUT` row, aggregated) is the gross-output
  vector used for technical coefficients.

### Coefficients and Leontief inverse

- **An** = `CIn %*% ginv(diag(GO))`; **Am** = `CIm %*% ginv(diag(GO))`
  (`etc/Modelo/base/coeficientes_tecnicos.R:13–39`), with `GO` = `Recurso["GO",]`
  (the aggregated raw `OUTPUT` row).
- **Z** = `ginv(I − An)`, the domestic Leontief inverse, 7×7
  (`etc/Modelo/base/matriz_impacto.R:16`). (Ghosh `G` at `:26–30` uses a DIFFERENT gross-output
  vector `rowSums(CIn)+rowSums(Fn)`; not used for the import aggregate.)

### Diagnosis assembly of the aggregate (`code/support_functions.R::prepare_data`)

Operating on the Stage-A `NIOTs` object (already `× exchange_rate / deflator_GDP`):

- **Ft** by component: `colSums` of `data[["Ft"]]`, renamed C=HFCE, G=GGFC, I=GFCF, X=EXPO,
  E=INVNT (`support_functions.R:27–46`). `Ft_total` = `rowSums(Ft)` (`:41–42`).
- **M_F** (imported final demand) = `colSums` of `data[["Fm"]]` (`:74–96`). Equals the Fm
  aggregate; per component `M_F = Fm`.
- **M_I** (embodied imported intermediates) = `colSums(Am %*% Z %*% Fn)` per final-demand
  component, where `Fn` is the domestic-final-demand matrix (7 sectors × 5 components)
  (`support_functions.R:100–131`).
- **M = M_F + M_I** (`:135`); **m = M / Ft** per component (`:139`).
- **GDP aggregate** = `Σ_j (1−m_j)·Ft_j` over j ∈ {C,I,E,G,X} (`support_functions.R:157–161`).

Algebraically, since `Ft = Fn + Fm`, `M_F = Fm`, and `M = Fm + M_I`:

```
Ft_total − M_total = Σ_j (Fn_j + Fm_j) − Σ_j (Fm_j + M_I_j)
                   = Σ_j (Fn_j − M_I_j)
                   = Fn_total − M_I_total.
```

The imported final demand `Fm` cancels exactly. The nominal GDP aggregate equals
**total domestic final demand minus embodied imported intermediates in that domestic
final demand**.

## Independent reconstruction procedure (for ticket #11)

Goal: recompute `Ft_total − M_total` for ONE country-year straight from `<ISO3><YEAR>dom.csv`,
without running Stage A. Because the scalar multiplier `exchange_rate/deflator_GDP` is uniform
per country-year: it cancels entirely in `Am` and `Z` (ratios), and scales `Fn` linearly, so it
factors out of the whole aggregate. Working directly on raw USD gives the **nominal USD**
aggregate; multiply the final scalar by `exchange_rate` for nominal national currency, or by
`exchange_rate/deflator_GDP` to match Stage A's constant-price national-currency figure.

Steps (all indices are `read.csv()` data.frame indices, i.e. CSV line − 1):

1. `df <- read.csv("<ISO3><YEAR>dom.csv")` → 95×56, labels in col 1, data in cols 2–56.
2. Load `agregador` (45×7 0/1) from `inputs/agregador.xlsx` and `n_setores = 7`.
3. **GO** (7-vector): `agg_cols(df[95, 2:46])` = raw `OUTPUT` row aggregated by
   `%*% agregador`. (Do NOT apply the deflator when targeting the nominal aggregate.)
4. **CIn** (7×7): `t(agregador) %*% as.matrix(df[1:45, 2:46]) %*% agregador`.
5. **CIm** (7×7): `t(agregador) %*% as.matrix(df[46:90, 2:46]) %*% agregador`.
6. **Fn** (7×5): `t(agregador) %*% as.matrix(df[1:45, 47:56])`, then apply the Un.R column
   adjustment: `HFCE += NPISH`, `EXPO += CONS_NONRES`, keep `{HFCE, GGFC, GFCF, INVNT, EXPO}`,
   drop `{NPISH, CONS_NONRES, DPABR, IMPO, TOTAL}`.
7. **An** = `CIn %*% ginv(diag(GO))`; **Am** = `CIm %*% ginv(diag(GO))`; **Z** = `ginv(I7 − An)`.
   (Zero out NA/Inf columns exactly as Stage A: `An/Am/Z[is.na] = 0`.)
8. **Fn_total** = `sum(Fn)` (all 7×5 entries).
9. **M_I_total** = `sum(Am %*% Z %*% Fn)` (all entries of the 7×5 product).
10. **Nominal aggregate (USD)** = `Fn_total − M_I_total`.
    (Equivalently, build `Fm` from `df[46:90, 47:56]` with the Um.R adjustment, `Ft = Fn+Fm`,
    `M_F = Fm`, and verify `sum(Ft) − sum(Fm) − M_I_total` returns the same number — a self-check
    that `Fm` cancels.)
11. Optional currency conversion of the single scalar: `× exchange_rate` (nominal national
    currency) or `× exchange_rate / deflator_GDP` (Stage-A constant-price national currency).

## Ambiguities / blockers

- **Aggregation-sensitivity of M_I (material)**. `M_I_total = sum(Am %*% Z %*% Fn)` is computed at
  the **7-sector** resolution (aggregation applied before An/Am/Z). Matrix inversion does not
  commute with aggregation, so `M_I_total` at 7 sectors differs from the 45-sector value. To
  reproduce the diagnosis figure exactly you MUST use the same `agregador.xlsx` mapping and
  `n_setores = 7`; a "cleaner" 45-sector computation would give a different number. This is the
  main constraint on an exact match.
- **Two distinct gross-output vectors**. `An/Am` use `GO` = raw `OUTPUT` row
  (`Recurso`, `coeficientes_tecnicos.R:13`), whereas the Ghosh block uses
  `rowSums(CIn)+rowSums(Fn)` (`matriz_impacto.R:26`). The import aggregate uses the first.
  `Z %*% Fn` equals the raw `OUTPUT` vector ONLY if the domestic row-balance
  `OUTPUT = rowSums(CIn) + rowSums(Fn)` holds; if the ICIO domestic accounts are not exactly
  closed (they generally are not, given `EXPO` and stat. discrepancies inside `Fn`),
  `M_I_total ≠ sum(CIm)`, so the `Am %*% Z %*% Fn` route must be used literally, not the
  `sum(CIm)` shortcut.
- **`ginv` pseudo-inverse on zero-output sectors**. Stage A uses `MASS::ginv` and then sets
  NA/Inf to 0 (`coeficientes_tecnicos.R:35,47`; `matriz_impacto.R:17`). A reconstruction must
  replicate the pseudo-inverse + zero-fill; using a plain `solve()` will diverge when any
  aggregated sector has `GO = 0`.
- **Currency/deflation, resolved but note-worthy**. Raw is nominal USD millions (confirmed from
  ReadMe). Stage-A outputs are constant-price national currency. The per-component import share
  `m` is scale-invariant, but the level `Ft_total − M_total` is NOT: the target currency/price
  basis must be stated explicitly. `exchange_rate` and `deflator_GDP` come from external OECD/WB
  files (`exchange_rate_OCDE.xlsx`, `deflator_GDP_OCDE.xlsx`, WB fallbacks) — needed only if the
  aggregate is wanted in national currency or constant prices, not for the raw-USD figure.
- **DPABR asymmetry** between Fn (dropped) and Fm (folded into HFCE) is deliberate; a
  reconstruction that treats the two blocks symmetrically would misstate consumption imports.
- **No independent unit metadata inside each `*dom.csv`**: units come only from the shared
  `IOTs_ReadMe.xlsx`. Cannot determine per-file whether any country-year deviates; assumed
  uniform current-USD-millions across all 1976 files.

## Sources

- `inputs/NATIODOMIMP/ARG1996dom.csv:1` (header/columns), `:2–46` (DOM rows), `:47–91` (IMP rows),
  `:92–96` (TXS/VALU/OUTPUT rows); row-label dump of the same file.
- `etc/Modelo/IOTs_ReadMe.xlsx` (units "current million USD"; label definitions).
- `etc/Modelo/compilacao.R:13–25` (year range, n_setores=7, source order).
- `etc/Modelo/base/base.R:5–10,25,28,31–48` (deflator_geral, TWN exclusion, country codes, load).
- `etc/Modelo/base/matrizes.R:5–33` (per-country/year loop, deflator selection, block sourcing).
- `etc/Modelo/base/titulos.R:10–34` (titulo, agregador, titulo_agregador).
- `etc/Modelo/base/Un.R:3–41` (CIn, Fn extraction + column adjustment).
- `etc/Modelo/base/Um.R:3–41` (CIm, Fm extraction + column adjustment).
- `etc/Modelo/base/Ut.R:4,13` (CI = CIn+CIm, Ft = Fn+Fm).
- `etc/Modelo/base/Recurso.R:4–25` (II/VA/GO from TXS/VALU/OUTPUT rows).
- `etc/Modelo/base/coeficientes_tecnicos.R:13–39` (GO, An, Am via ginv).
- `etc/Modelo/base/matriz_impacto.R:3,16,26` (Z = ginv(I−An); Ghosh's alternate output vector).
- `etc/Modelo/base/exchange_rate.R:1`; `etc/Modelo/base/deflator.R` (external rate/deflator sources).
- `etc/Modelo/agregador.xlsx` (45×7 membership; 7 aggregate sector names).
- `code/support_functions.R:12,27–46,74–96,100–135,139,157–161` (Ft, M_F, M_I=Am·Z·Fn, m, GDP identity).
