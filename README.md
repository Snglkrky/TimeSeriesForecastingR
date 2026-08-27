# SARIMA Forecasting of U.S. Natural Gas Import Prices

A seasonal time-series analysis in R: identifying, fitting, diagnosing and forecasting a SARIMA model for monthly
U.S. natural gas import prices from 1989 onward.

## Objective

Build a statistically valid seasonal ARIMA model for a non-stationary, seasonally structured price series, and produce
a 24-month ahead forecast with prediction intervals.

## Data

Monthly U.S. natural gas import prices, January 1989 onward, published by the U.S. Energy Information Administration
(<https://www.eia.gov/dnav/ng/hist/n9100us3m.htm>), stored here as `importsgasprice.xlsx`.

## Method

1. **Exploration** — the series is converted to a monthly `ts` object and decomposed additively into trend, seasonal
   and remainder components.
2. **Stationarity testing** — three complementary tests are applied at every transformation stage: Augmented
   Dickey–Fuller and Phillips–Perron (null: unit root) alongside KPSS (null: stationarity). Using tests with opposing
   null hypotheses avoids concluding stationarity purely from a failure to reject.
3. **Variance stabilisation** — a logarithmic transformation is applied, since the amplitude of the fluctuations grows
   with the level of the series.
4. **Differencing** — non-seasonal (`d = 1`) and seasonal (`D = 1`, lag 12) differences are taken; ACF and PACF plots
   are inspected after each step to avoid over-differencing.
5. **Model selection** — candidate specifications from `auto.arima()` are compared against manually identified models
   using **AICc**, which corrects the AIC small-sample bias.
6. **Diagnostics** — the residuals of the selected model are tested for white-noise behaviour (Ljung–Box test,
   residual ACF, normality check).
7. **Forecasting** — a 24-month forecast with confidence intervals is produced from the accepted model.

## Result

The selected specification is **SARIMA(0,1,1)(0,1,1)[12]** on the log-transformed series. Its residuals are consistent
with white noise, so the model is statistically adequate for forecasting. The fitted model and its 24-month forecast
are presented in `SARIMA.pdf`.

## Repository contents

| File | Description |
|---|---|
| `SARIMA.R` | Full analysis script: decomposition, stationarity testing, differencing, model selection, diagnostics, forecasting. |
| `SARIMA.Rmd` | R Markdown source of the report. |
| `SARIMA.pdf` | Compiled report with all figures and test output. |
| `importsgasprice.xlsx` | Source data (EIA). |

## Tools

R · `readxl` · `tseries` · `forecast` · `ggplot2` · R Markdown

## Reproducing the analysis

1. Install the packages listed above.
2. Update the path in `read_xlsx()` to point to your local copy of `importsgasprice.xlsx`.
3. Run `SARIMA.R`, or knit `SARIMA.Rmd` to regenerate `SARIMA.pdf`.

> The report is currently written in Turkish. An English edition is planned.

## Author

**Songül Karakaya** — M.Sc. student in Data Science, Dokuz Eylül University
[LinkedIn](https://www.linkedin.com/in/song%C3%BCl-karakaya-a03257322) · [GitHub](https://github.com/Snglkrky)
