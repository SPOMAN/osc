## ---- message=FALSE------------------------------------------------------
library(tidyverse)
library(gRaphene)

cf_folder <- system.file("extdata/graphene_curve_fit_export", package = "gRaphene")
data_cf <- gr_cf_read(cf_folder)

## ------------------------------------------------------------------------
data_cf_filtered <- data_cf %>% filter(`G int` > 50) %>% select(-starts_with('SiO2'))

knitr::kable(
  gr_cf_summary(data_cf_filtered), 
  digits = c(0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 2, 2)
)

## ---- fig.show='hold'----------------------------------------------------
data_cf_small <- data_cf %>% select(x, y, `G int`) %>% rename(value = `G int`)
gr_cf_plot(df = data_cf_small, name = 'G int')
gr_cf_plot(df = data_cf_small, name = 'G int', scalebar = 2, palette = "Greys")

## ---- fig.width = 7, fig.height=8----------------------------------------
data_cf %>%
  mutate(condition = `G int` > 50) %>% 
  mutate(`2D FWHM` = ifelse(condition, `2D FWHM`, NA)) %>% 
  mutate(`D FWHM` = ifelse(condition, `D FWHM`, NA)) %>%  
  mutate(`G FWHM` = ifelse(condition, `G FWHM`, NA)) %>%  
  mutate(`D/G-ratio` = ifelse(condition, `D/G-ratio`, NA)) %>%
  mutate(`2D/G-ratio` = ifelse(condition, `2D/G-ratio`, NA)) %>% 
  gr_cf_plot_all()

## ---- fig.width = 7, fig.height=3, message=FALSE-------------------------
curvefit <- system.file("extdata/curvefit_data.txt", package = "gRaphene")
spectrum <- system.file("extdata/spectrum_example.txt", package = "gRaphene")

gr_cf_fit_plot(spectrum, curvefit)

## ---- fig.width = 7, fig.height=3, message=FALSE-------------------------
gr_cf_ann <- gr_cf_fit_add(spectrum, curvefit) %>% 
  select(-intensity, -envelope) %>% 
  gather(key = "peak", value = "intensity", -wavenumber) %>% 
  group_by(peak) %>% 
  filter(intensity == max(intensity))

gr_cf_fit_plot(spectrum, curvefit) +
  ggthemes::theme_few() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  scale_fill_brewer(palette = "Set3", guide = guide_legend(nrow = 1)) +
  geom_text(data = gr_cf_ann, aes(x = wavenumber, y = intensity, label = peak), nudge_y = 40, nudge_x = 10)

## ---- message=FALSE------------------------------------------------------
fit_data <- gr_cf_fit_add(spectrum, curvefit)

knitr::kable(head(fit_data, 5), digits = 3)

