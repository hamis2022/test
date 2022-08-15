library(tidyverse)
library(knitr)
pengiuns <- read_csv("pengiuns.csv")

pengiuns %>% 
  dplyr::select(c(3:5)) %>% 
  head(2) %>% rename(length=bill_length_mm,
                depth=bill_depth_mm)

pengiuns %>%
  dplyr::select_if( is.numeric )%>%
  head(2) %>% kable()

pengiuns %>%
  dplyr::select(names(.)[grepl('_mm' , names(.))] )%>%
  head(2) %>% kable()


pengiuns <- pengiuns %>%
  pivot_longer(-c(island, sex, year, species),
               names_to = 'parameter',
               values_to = 'result') %>%
  dplyr::mutate(parameter=gsub('bill_|_mm|_g', '', parameter),
                parameter=case_when(grepl('flipper', parameter)~'flipper',
                                    grepl('body', parameter)~'bodymass',
                                    TRUE~parameter),
                parameter=factor(parameter, levels=c( 'depth', 'length', 'flipper', 'bodymass' )
                ),
                result=log10(result),
                year=factor(year)
  ) %>%
  dplyr::filter(!is.na(result))







(ggplot(pengiuns ,
        aes(x=parameter, y=result, colour=year ))+
    geom_jitter(height=0)
  # + facet_wrap(~species+sex)+
  + facet_grid(species~sex)+
    labs(x='Parameter', y='Result [log10(mm or g)]')+
    theme_light()+
    theme(axis.text.x = element_text(angle = 90),
          strip.text =element_text(colour = 'black'),
          strip.background = element_rect(fill='skyblue'))
)

