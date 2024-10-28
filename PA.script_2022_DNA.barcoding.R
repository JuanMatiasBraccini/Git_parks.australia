
rm(list=ls(all=TRUE))

User="Matias"
# User="Robiul"


if(!exists('handl_OneDrive'))
{
  if(User=="Matias") source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')

  if(User=="Robiul")
  {
    handl_OneDrive=function(x)paste('C:/Users',Usr,'OneDrive - Department of Primary Industries and Regional Development/Matias',x,sep='/')
  }
}

library(tidyverse)
library(readxl)
library(stringr)
library(janitor)
library(Hmisc)
library(lubridate)
library(ggpubr)
library(grid)   # for the textGrob() function
library(gridExtra) 
library(ggsankey)
options(stringsAsFactors = FALSE,dplyr.summarise.inform = FALSE) 

# Input data --------------------------------------------------------------------
Original.Sampling <- read_excel(handl_OneDrive("Parks Australia/2022_project/Data/DNA barcoding/F&C sampling_Private.xlsx"), sheet = "Original Sampling",skip = 0)
Resample.Metro <- read_excel(handl_OneDrive("Parks Australia/2022_project/Data/DNA barcoding/F&C sampling_Private.xlsx"), sheet = "Resample Metro",skip = 0)
Opportunistic <- read_excel(handl_OneDrive("Parks Australia/2022_project/Data/DNA barcoding/F&C sampling_Private.xlsx"), sheet = "Opportunistic",skip = 0)
IUCN=read.csv(handl_OneDrive("Parks Australia/2022_project/Data/DNA barcoding/status.csv"))
Shop.location=read.csv(handl_OneDrive("Parks Australia/2022_project/Data/DNA barcoding/Shop location.csv"))
Origin.all.sequenced.species=read.csv(handl_OneDrive("Parks Australia/2022_project/Data/DNA barcoding/All.sequenced.species.csv"))

# Manipulate data --------------------------------------------------------------------
Original.Sampling=Original.Sampling%>%
  filter(!is.na(Serial))
names(Original.Sampling)=word(names(Original.Sampling),1,sep = "\\...")

Resample.Metro=Resample.Metro%>%
  filter(!is.na(Serial))
names(Resample.Metro)=word(names(Resample.Metro),1,sep = "\\...")

Opportunistic=Opportunistic%>%
  filter(!is.na(Serial))
names(Opportunistic)=word(names(Opportunistic),1,sep = "\\...")

Resample.Metro=Resample.Metro%>%
  dplyr::select(-c("Sample Collected (date)","Number of staff at shop","Advertised product label",
                   "Do you know what fish this is?","Advertised product origin",
                   "Do you know where the fish comes from?","Price","Comments"))

Original.Sampling=clean_names(Original.Sampling)
Resample.Metro=clean_names(Resample.Metro)
Opportunistic=clean_names(Opportunistic)

Original.Sampling=Original.Sampling%>%
            rename(sample_date=sample_collected_date,
                   comment=where_do_you_get_your_fish_from_na)%>%
            dplyr::select(-c(chip_rating,fish_rating,x,key))%>%
            data.frame
#            mutate(sample_date=case_when(sample_date=="Big Reds fish and chips? 3/5/23"~"03/5/23",
#                   TRUE~sample_date))

Resample.Metro=Resample.Metro%>%
          rename(sample_date=resample_date,
                 comment=comment_resample)%>%
          dplyr::select(-label_number_resample)%>%
          data.frame
     
Opportunistic=Opportunistic%>%
              mutate(shop_opening_hours_weekdays=NA,
                     shop_opening_hours_weekends=NA)%>%
              rename(shop=shop_name,
                     sample_date=sample_collected_date,
                     comment=comments)%>%
          dplyr::select(-c(source,up_to_label_number))%>%
          relocate(names(Resample.Metro))%>%
          data.frame

#remove some samples due to inadequate Sequenced length (below 645 base pairs)
drop.samples=c("S084","S086","S096","S116")
Original.Sampling=Original.Sampling%>%filter(!label_number%in%drop.samples)
drop.samples=c("S008","S025i","S065ii")
Resample.Metro=Resample.Metro%>%filter(!label_number%in%drop.samples)


DATA=rbind(Original.Sampling,Resample.Metro,Opportunistic)%>%
  mutate(year=year(sample_date),
        month=month(sample_date),
        day=day(sample_date),
         shop=tolower(shop),
         advertised_product_label=tolower(advertised_product_label),
         do_you_know_what_fish_this_is=tolower(do_you_know_what_fish_this_is),
         advertised_product_origin=tolower(advertised_product_origin),
         do_you_know_where_the_fish_comes_from=tolower(do_you_know_where_the_fish_comes_from),
         advertised_product_label_original=advertised_product_label,
         advertised_product_label=case_when(advertised_product_label=="shakr"~"shark",
                                            advertised_product_label=="shark(local)"~"shark (local)",
                                            advertised_product_label=="no shark"~"fish and chips",
                                            advertised_product_label=="shark  bronzy"~"shark bronzy",
                                            advertised_product_label%in%c("wa shark","shark(wa)")~"shark (wa)",
                                            TRUE~advertised_product_label),
         advertised_product_origin_original=advertised_product_origin,
         advertised_product_origin=case_when(advertised_product_origin%in%c("na","Na","NA")~NA,
                                            TRUE~advertised_product_origin),
         do_you_know_where_the_fish_comes_from_original=do_you_know_where_the_fish_comes_from,
         do_you_know_where_the_fish_comes_from=case_when(do_you_know_where_the_fish_comes_from%in%c("na","language barrier")~NA,
                                                         do_you_know_where_the_fish_comes_from%in%c("local wa")~"local, wa",
                                                         do_you_know_where_the_fish_comes_from%in%c("\"from here\"")~"wa",
                                                         do_you_know_where_the_fish_comes_from%in%c("unknown","don't know","didn't know")~"no",
                                                         do_you_know_where_the_fish_comes_from%in%c("from another country")~"imported",
                                             TRUE~do_you_know_where_the_fish_comes_from),
         do_you_know_what_fish_this_is_original=do_you_know_what_fish_this_is,
         do_you_know_what_fish_this_is=case_when(do_you_know_what_fish_this_is%in%c("bronze","bronze whaler","bronzey",
                                                                                    "bronzie","bronzy")~"bronze whaler",
                                                 do_you_know_what_fish_this_is%in%c("gummy")~"gummy shark",
                                                 do_you_know_what_fish_this_is%in%c("saint josephs")~"cape elephant fish",
                                                 do_you_know_what_fish_this_is%in%c("na","NA","sky blue","fish",
                                                                                    "fish didn't know what it was")~NA,
                                                 do_you_know_what_fish_this_is%in%c("unknown","didn't know",
                                                                                    "don't know","don't know, shark local?")~'no',
                                                 TRUE~do_you_know_what_fish_this_is),
         original_species_after_sequencing_original=original_species_after_sequencing,
         original_species_after_sequencing=case_when(grepl('Blue grenadier',original_species_after_sequencing)~"Blue grenadier (Macruronus novaezelandiae)",
                                                     grepl('Cape elephantfish',original_species_after_sequencing)~"Cape elephantfish (Callorhinchus capensis)",
                                                     grepl('Dark ghost shark',original_species_after_sequencing)~"Dark ghost shark (Hydrolagus novaezealandiae)",
                                                     grepl('Merluccius paradoxus',original_species_after_sequencing)~"Deep water cape hake (Merluccius paradoxus)",
                                                     grepl('Scomberomorus commerson',original_species_after_sequencing)~"Narrow-barred Spanish mackerel (Scomberomorus commerson)",
                                                     TRUE~original_species_after_sequencing),
         Common_name_sequenced=trimws(word(original_species_after_sequencing,sep = "\\(")),
         Scientific_name_sequenced=stringr::str_extract(string = original_species_after_sequencing,pattern = "(?<=\\().*(?=\\))"))

#Add IUCN status
RiskColors=c("Not evaluated"="grey85",'Negligible risk'="cornflowerblue",'Low risk'="chartreuse3",
             'Medium risk'="yellow1",'High risk'="orange",'Severe risk'="brown3")
#colors based on https://nc.iucnredlist.org/redlist/resources/files/1646067752-FINAL_IUCN_Red_List_colour_chart.pdf
IUCN.colors=c("Not evaluated"="grey85","Least concern"="#60C659","Near threatened"="#CCE226",
              "Vulnerable"="#F9E814","Endangered"="#FC7F3F","Critically endangered"="#D81E05")
IUCN.Endangered.categories=c("Vulnerable","Endangered","Critically endangered")

Mislabelling.col.vec=c(No='forestgreen',Yes='brown4')
DATA=DATA%>%
  left_join(IUCN%>%
              dplyr::select(-Common_name_sequenced)%>%
              mutate(IUCN.status=IUCN.status_Australia,
                     IUCN.status=ifelse(is.na(IUCN.status)|IUCN.status=="","Not evaluated",IUCN.status),
                     WOE_WA=ifelse(is.na(WOE_WA)|WOE_WA=="","Not evaluated",WOE_WA),
                     WOE_WA=factor(WOE_WA,levels=names(RiskColors)),
                     IUCN.status=factor(IUCN.status,levels=names(IUCN.colors)),
                     IUCN.status_global=factor(IUCN.status_global,levels=names(IUCN.colors))),
            by='Scientific_name_sequenced')


#Determine match between label and sequencing
#note (as per Sharrad et al 2023):
#     Correctly labelled = identification by the retailer and molecular methods agreed. 
#     Mislabelled/Substitution = retailer ID was different to the species identified through DNA barcoding.
#     Ambiguously labelled = ambiguous term used by retailer (e.g. 'shark','fish') and no other information available.

  #DNA barcoded species
Shark.species=c("carpet shark","copper shark","dusky shark","gummy shark","sandbar shark",
                "shortfin mako shark", "smooth hammerhead","spinner shark",
                "spot-tail shark","tiger shark","whiskery shark")
Teleost.species=c('blue grenadier','deep water cape hake','narrow-barred spanish mackerel')   
Chimaera.species=c('cape elephantfish','dark ghost shark')
All.sequenced.species=tolower(sort(unique(DATA$Common_name_sequenced)))
stopifnot(length(which(!All.sequenced.species%in%c(Shark.species,Teleost.species,Chimaera.species)))==0)

  #Labels
Shark.generic=c("albany shark","local shark","shark","shark (local)","shark (wa)","shark and chips")
Bronzies=c("bronze shark","bronze whaler (local)","bronze whaler local","bronze whaler shark","shark (bronze whaler)",
           "bronze whaler shark - local","bronzey/flake","bronze whaler(flake)","bronzey","shark bronzy","shark wa bronze whaler")
Bronzy_gummy=c("bronzey/gummy","local shark (gummy or bronze)","shark (bronze or gummy)")
Gummies=c("flake (gummy shark)","gummy","gummy shark","gummy shark (local)",
          "gummy shark (wa)","local gummy shark","shark (gummy)")
Flake=c("flake","flake (local)")
Ambiguous.term=c(Shark.generic,"fish and chips")
DATA=DATA%>%
  mutate(Common_name_sequenced=tolower(Common_name_sequenced),
         Ambiguous.labelling=case_when(advertised_product_label%in%Ambiguous.term~"Yes",
                                       TRUE~"No"),
         Match_label_DNA_mislabelling=case_when(grepl('hammerhead',advertised_product_label) & !grepl('hammerhead',Common_name_sequenced)~ "Yes",
                                                grepl('bronz',advertised_product_label) & !grepl(paste(c('dusky','copper'),collapse="|"),Common_name_sequenced)~ "Yes",
                                                grepl('hoki',advertised_product_label) & !Common_name_sequenced=="blue grenadier"~ "Yes",
                                                grepl('flake',advertised_product_label) & !Common_name_sequenced=="gummy shark"~ "Yes",
                                                grepl('carpet',advertised_product_label) & !grepl('carpet',Common_name_sequenced)~ "Yes",
                                                grepl('gummy',advertised_product_label) & !grepl('gummy',Common_name_sequenced)~ "Yes",
                                                advertised_product_label%in%c("bronzey/gummy","bronzey/flake","local shark (gummy or bronze)",
                                                                              "shark (bronze or gummy)","bronze whaler(flake)") &
                                                  (!Common_name_sequenced%in%c("dusky shark","gummy shark","copper shark")) ~ "Yes",
                                                grepl('whiskery',advertised_product_label) & !Common_name_sequenced=="whiskery shark"~ "Yes",
                                                advertised_product_label%in%c("albany shark","local shark","shark",
                                                                              "shark (local)","shark (wa)","shark and chips") &
                                                  !Common_name_sequenced%in%Shark.species ~ "Yes",
                                                TRUE~"No"))

DATA=DATA%>%
  mutate(Match_label_DNA_mislabelling=case_when(advertised_product_label%in%c("bronzey/gummy","bronzey/flake",
                                                                              "local shark (gummy or bronze)","bronze whaler(flake)") &
                                                  Common_name_sequenced%in%c("dusky shark","gummy shark","copper shark") ~ "No",
                                                TRUE~Match_label_DNA_mislabelling))
DATA=DATA%>%
  mutate(report_label=case_when(advertised_product_label=="albany hammerhead shark"~"hammerhead shark",
                                advertised_product_label%in%Shark.generic~"shark",
                                advertised_product_label%in%Bronzies~"bronze whaler",
                                advertised_product_label%in%Bronzy_gummy~"bronze/gummy",
                                advertised_product_label%in%Gummies~"gummy shark",
                                advertised_product_label%in%Flake~"flake",
                                advertised_product_label=="whiskery shark local" ~"whiskery shark",
                                advertised_product_label=="carpet shark burguer" ~"carpet shark",
                                TRUE~advertised_product_label))

table(DATA$Match_label_DNA_mislabelling)

  #add sequenced species origin and define if advertised origin matches
Aussie.origin=c('local waters','local','wa','bunbury','wa,nz','local bunbury','albany','aus and nz','local, albany')
DATA=DATA%>%
  left_join(Origin.all.sequenced.species%>%dplyr::select(-Source),
            by=c('Common_name_sequenced','Scientific_name_sequenced'))
DATA=DATA%>%
  mutate(Mismatch_label_DNA_origin=case_when(Distribution=='Global'~NA,
                                             is.na(advertised_product_origin)~NA,
                                          Distribution=='Australia' & advertised_product_origin%in%Aussie.origin~"No",
                                          Distribution=='South Africa' & advertised_product_origin%in%'south africa'~"No",
                                          Distribution=='South Africa' & advertised_product_origin%in%Aussie.origin~"Yes",
                                          Distribution=='Australia' & !advertised_product_origin%in%Aussie.origin~"Yes"))

  #Indentify resampled shops
Table.shops.replicates=table(DATA$shop,useNA = 'ifany')
Table.shops.replicates=subset(Table.shops.replicates,Table.shops.replicates>1)
DATA=DATA%>%
        mutate(Replicate.sample=ifelse(shop%in%names(Table.shops.replicates),'YES','NO'),
               Replicate.sample=ifelse(shop=="kim's fish & chips",'NO',   #3 different shops same name
                                ifelse(shop=="santorini fish and chips"   & post_code==6053,'NO',
                                ifelse(shop=="cicerellos",'NO',   #2 different shops same name
                                Replicate.sample))))

# Analysis ----------------------------------------------------------------
HNDL=handl_OneDrive('Analyses/Parks Australia/outputs/2022 Project/')
le.paste=function(x) paste(HNDL,x,sep='')

#Plot mislabeling    
Tab1=DATA%>%
  mutate(report_label=capitalize(report_label),
         Ambiguous.labelling=ifelse(Ambiguous.labelling=='Yes','Ambiguous','Unambiguous'))%>%
  group_by(report_label,Match_label_DNA_mislabelling,Ambiguous.labelling)%>%
  tally()
Tab1.nsp=DATA%>%
  mutate(report_label=capitalize(report_label),
         Ambiguous.labelling=ifelse(Ambiguous.labelling=='Yes','Ambiguous','Unambiguous'))%>%
  group_by(report_label,Match_label_DNA_mislabelling,Ambiguous.labelling,Common_name_sequenced)%>%
  tally()%>%
  spread(Common_name_sequenced,n,fill ='')
write.csv(Tab1%>%left_join(Tab1.nsp,by=c('report_label','Match_label_DNA_mislabelling','Ambiguous.labelling')),
          le.paste("Table_Mislabelling_ambiguous.csv"),row.names = F)

Tab1%>%
  ggplot() +
  geom_bar(aes(x=report_label, y=n, fill = Match_label_DNA_mislabelling),
           stat = "identity")+
  facet_wrap(~Ambiguous.labelling)+
  theme_bw()+
  theme(legend.position = 'top',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        strip.text.x = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  coord_flip()+xlab('Product label')+ylab('Number of samples')+
  scale_fill_discrete(name = "Mislabelled",type=Mislabelling.col.vec)
ggsave(le.paste("Mislabelling_ambiguous.tiff"),width = 9,height = 7,compression = "lzw")


#Plot Sankey Label and Sequenced
write.csv(DATA[,c('report_label','Common_name_sequenced')]%>%arrange(report_label,Common_name_sequenced),
          le.paste("Table_Sankey_label_sequenced.csv"),row.names = F)
df <- DATA %>%
  mutate(report_label=capitalize(report_label),
         Common_name_sequenced=capitalize(Common_name_sequenced))%>%
  make_long(report_label,Common_name_sequenced)%>%
  mutate(x=ifelse(x=='report_label','Product label',
                  ifelse(x=='Common_name_sequenced','Sequenced species',
                         NA)))
#sort(unique(df$node))
 species_colors=c("blue grenadier"='cadetblue4',"bronze whaler"='chocolate1',"bronze/gummy"='darkgoldenrod4',
                  "cape elephantfish"='darkolivegreen3',"carpet shark"='darkgoldenrod2', 
                  "copper shark"='chocolate3',"dark ghost shark"='forestgreen',"deep water cape hake"='darkslategray3',
                  "dusky shark"='darkorange4',"fish (hoki)"='cadetblue3',"fish and chips"='deepskyblue4',
                  "flake" ='brown',"gummy shark"='brown3',"hammerhead shark"='darkorange2',
                  "narrow-barred spanish mackerel"='cornflowerblue',"sandbar shark"='firebrick',"shark"='coral4',
                  "shortfin mako shark"='firebrick1', "smooth hammerhead"='firebrick3',"spinner shark"='goldenrod3',
                  "spot-tail shark"='goldenrod4', "tiger shark"='goldenrod1',"whiskery shark"='darkorange' )
names(species_colors)=capitalize(names(species_colors))
species_colors=setNames(species_colors, levels(as.factor(names(species_colors))))
p=ggplot(df, aes(x = x, 
                 next_x = next_x, 
                 node = node, 
                 next_node = next_node,
                 fill = factor(node),
                 label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 4, color = 'white') +  
  theme_sankey(base_size = 16) +
  guides(fill = guide_legend(title = "Title"))+
  theme(legend.position = "none")+xlab('')
p+scale_fill_manual(values = species_colors)
ggsave(le.paste("Sankey_label_sequenced.tiff"),width = 6,height = 8,compression = "lzw")

#Plot mismatch origin
Tab1=DATA%>%
  filter(!is.na(Mismatch_label_DNA_origin))%>%
  mutate(report_label=capitalize(report_label),
         Ambiguous.labelling=ifelse(Ambiguous.labelling=='Yes','Ambiguous','Unambiguous'))%>%
  filter(Ambiguous.labelling=='Unambiguous')%>%
  group_by(report_label,Mismatch_label_DNA_origin,Ambiguous.labelling)%>%
  tally()

Tab1.nsp=DATA%>%
  mutate(report_label=capitalize(report_label),
         Ambiguous.labelling=ifelse(Ambiguous.labelling=='Yes','Ambiguous','Unambiguous'))%>%
  group_by(report_label,Mismatch_label_DNA_origin,Ambiguous.labelling,Common_name_sequenced)%>%
  tally()%>%
  spread(Common_name_sequenced,n,fill ='')

Tab1.orign=DATA%>%
  mutate(report_label=capitalize(report_label),
         Ambiguous.labelling=ifelse(Ambiguous.labelling=='Yes','Ambiguous','Unambiguous'))%>%
  group_by(report_label,Mismatch_label_DNA_origin,Ambiguous.labelling,Distribution)%>%
  tally()%>%
  spread(Distribution,n,fill ='')

write.csv(Tab1%>%
            left_join(Tab1.nsp,by=c('report_label','Mismatch_label_DNA_origin','Ambiguous.labelling'))%>%
            left_join(Tab1.orign,by=c('report_label','Mismatch_label_DNA_origin','Ambiguous.labelling')),
          le.paste("Table_Mismatch.origin_unambiguous.csv"),row.names = F)
Tab1%>%
  ggplot() +
  geom_bar(aes(x=report_label, y=n, fill = Mismatch_label_DNA_origin),
           stat = "identity")+
  facet_wrap(~Ambiguous.labelling)+
  theme_bw()+
  theme(legend.position = 'top',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        strip.text.x = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  coord_flip()+xlab('Product label')+ylab('Number of samples')+
  scale_fill_discrete(name = "Mismatched origin",type=Mislabelling.col.vec)
ggsave(le.paste("Mismatch.origin_unambiguous.tiff"),width = 9,height = 7,compression = "lzw")


#Plot Status
Lgn.tit=12
Lgn.txt=9
p_IUCN_Oz=DATA%>%
  mutate(Common_name_sequenced=capitalize(Common_name_sequenced))%>%
  group_by(Common_name_sequenced,IUCN.status)%>%
  tally()%>%
  ggplot() +
  geom_bar(aes(x=Common_name_sequenced, y=n, fill = IUCN.status),
           stat = "identity")+
  theme_bw()+
  theme(legend.position = 'top',
        legend.title = element_text(size = Lgn.tit),
        legend.text = element_text(size = Lgn.txt),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  coord_flip()+xlab('Sequenced species')+ylab('Occurrence')+
  scale_fill_manual("IUCN",values=IUCN.colors,drop=FALSE)+ 
  guides(fill = guide_legend(nrow = 1))

p_IUCN_global=DATA%>%
  mutate(Common_name_sequenced=capitalize(Common_name_sequenced))%>%
  group_by(Common_name_sequenced,IUCN.status_global)%>%
  tally()%>%
  ggplot() +
  geom_bar(aes(x=Common_name_sequenced, y=n, fill = IUCN.status_global),
           stat = "identity")+
  theme_bw()+
  theme(legend.position = 'top',
        legend.title = element_text(size = Lgn.tit),
        legend.text = element_text(size = Lgn.txt),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  coord_flip()+xlab('Sequenced species')+ylab('Occurrence')+
  scale_fill_manual("IUCN",values=IUCN.colors,drop=FALSE)+ 
  guides(fill = guide_legend(nrow = 1))

p_WoE=DATA%>%
  mutate(Common_name_sequenced=capitalize(Common_name_sequenced))%>%
  group_by(Common_name_sequenced,WOE_WA)%>%
  tally()%>%
  ggplot() +
  geom_bar(aes(x=Common_name_sequenced, y=n, fill = WOE_WA),
           stat = "identity")+
  theme_bw()+
  theme(legend.position = 'top',
        legend.justification='right',
        legend.title = element_text(size = Lgn.tit),
        legend.text = element_text(size = Lgn.txt),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  coord_flip()+xlab('Sequenced species')+ylab('Occurrence')+
  scale_fill_manual("Risk",values=RiskColors,drop=FALSE)+ 
  guides(fill = guide_legend(nrow = 1))

 figure.IUCN <- ggarrange(p_IUCN_global + rremove("ylab") + rremove("xlab"),
                          p_IUCN_Oz + rremove("ylab") + rremove("y.text") + rremove("xlab"),
                     labels = c('Global','Australia'),
                     ncol = 2, nrow = 1, widths = c(1, 0.5),
                     common.legend = TRUE)

figure=grid.arrange(grobs=list(figure.IUCN,p_WoE+ rremove("ylab") + rremove("xlab")),
             layout_matrix = rbind(c(1,1),
                                   c(2,2)))
annotate_figure(figure, 
                left = text_grob("Sequenced species", size=16, rot = 90, vjust = 1),
                bottom = text_grob("Occurrence", size=16))
ggsave(le.paste("Status.tiff"),width = 8,height = 8,compression = "lzw")


#Plot 'do_you_know_what_fish_this_is'
dummy=DATA%>%
    filter(Ambiguous.labelling=='Yes')%>%
    filter(!is.na(do_you_know_what_fish_this_is))%>%
    filter(!do_you_know_what_fish_this_is=='no')%>%
    mutate(Match_do.you.know.fish_DNA_mislabelling=case_when(
        do_you_know_what_fish_this_is=='blacktip shark or reef shark' & Common_name_sequenced=='smooth hammerhead'~"Yes",
        do_you_know_what_fish_this_is=='bronze whaler' & !grepl(paste(c('dusky','copper'),collapse="|"),Common_name_sequenced) ~"Yes",
        do_you_know_what_fish_this_is=='cape elephant fish' & !Common_name_sequenced=='cape elephantfish'~"Yes",
        grepl('flake',do_you_know_what_fish_this_is) & !Common_name_sequenced=="gummy shark"~ "Yes",
        do_you_know_what_fish_this_is=='gummy or bronzie' & !grepl(paste(c('dusky','copper','gummy'),collapse="|"),Common_name_sequenced)~"Yes",
        do_you_know_what_fish_this_is=='gummy or mako' & !grepl(paste(c('mako','gummy'),collapse="|"),Common_name_sequenced)~"Yes",
        do_you_know_what_fish_this_is=='gummy or whiskery' & !grepl(paste(c('whiskery','gummy'),collapse="|"),Common_name_sequenced)~"Yes",
        do_you_know_what_fish_this_is=='gummy shark' & !grepl(paste(c('gummy'),collapse="|"),Common_name_sequenced)~"Yes",
        do_you_know_what_fish_this_is=='hake' & !grepl(paste(c('hake'),collapse="|"),Common_name_sequenced)~"Yes",
        do_you_know_what_fish_this_is=='hammerhead' & !grepl(paste(c('hammerhead'),collapse="|"),Common_name_sequenced)~"Yes",
        do_you_know_what_fish_this_is=='hoki' & !Common_name_sequenced=='blue grenadier'~"Yes",
        do_you_know_what_fish_this_is=='shark' & !grepl(paste(c('shark'),collapse="|"),Common_name_sequenced)~"Yes",
        do_you_know_what_fish_this_is=='snapper' & !grepl(paste(c('snapper'),collapse="|"),Common_name_sequenced)~"Yes",
        do_you_know_what_fish_this_is=='whiskery' & !grepl(paste(c('whiskery'),collapse="|"),Common_name_sequenced)~"Yes",
                                             TRUE~"No"))%>%
    mutate(do_you_know_what_fish_this_is=capitalize(do_you_know_what_fish_this_is),
           Ambiguous.labelling=ifelse(Ambiguous.labelling=='Yes','Ambiguous','Unambiguous'))
Tab1=dummy%>%
    group_by(do_you_know_what_fish_this_is,Match_do.you.know.fish_DNA_mislabelling,Ambiguous.labelling)%>%
    tally()
Tab1.nsp=dummy%>%
    group_by(do_you_know_what_fish_this_is,Match_do.you.know.fish_DNA_mislabelling,Ambiguous.labelling,Common_name_sequenced)%>%
    tally()%>%
    spread(Common_name_sequenced,n,fill ='')
write.csv(Tab1%>%left_join(Tab1.nsp,by=c('do_you_know_what_fish_this_is','Match_do.you.know.fish_DNA_mislabelling','Ambiguous.labelling')),
          le.paste("Table_Mislabelling_do_you_know_what_fish_this_is.csv"),row.names = F)  

Tab1%>%
  ggplot() +
  geom_bar(aes(x=do_you_know_what_fish_this_is, y=n, fill = Match_do.you.know.fish_DNA_mislabelling),
           stat = "identity")+
  facet_wrap(~Ambiguous.labelling)+
  theme_bw()+
  theme(legend.position = 'top',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        strip.text.x = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  coord_flip()+xlab('Product name given by retailer')+ylab('Number of samples')+
  scale_fill_discrete(name = "Misleading answer",type=Mislabelling.col.vec)
ggsave(le.paste("Mislabelling_ambiguous_Do_you_know_what_fish_this_is.tiff"),width = 9,height = 7,compression = "lzw")

  
#Plot 'do_you_know_where_the_fish_comes_from
Aussie.origin.do.u.know=c(Aussie.origin,'augusta','between albany and esperance','esperance','local perth','local, wa','perth')
dummy=DATA%>%
  filter(is.na(advertised_product_origin))%>%
  filter(!do_you_know_where_the_fish_comes_from=='no')%>%
  filter(!is.na(do_you_know_where_the_fish_comes_from))%>%
  filter(!Distribution=='Global')%>%
  mutate(Mismatch_do.you.know.origin_DNA_origin=case_when(Distribution=='South Africa' & grepl('africa',do_you_know_where_the_fish_comes_from)~"No",
                                                          Distribution=='South Africa' & grepl('not local',do_you_know_where_the_fish_comes_from)~"No",
                                                          Distribution=='South Africa' & grepl('imported',do_you_know_where_the_fish_comes_from)~"No",
                                                          Distribution=='Australia' & do_you_know_where_the_fish_comes_from%in%Aussie.origin.do.u.know~"No",
                                                          TRUE~"Yes"),
         Ambiguous.labelling=ifelse(Ambiguous.labelling=='Yes','Ambiguous','Unambiguous'),
         report_label=capitalize(report_label))
Tab1=dummy%>%
  group_by(report_label,Mismatch_do.you.know.origin_DNA_origin,Ambiguous.labelling)%>%
  tally()
Tab1.nsp=dummy%>%
          group_by(report_label,Mismatch_do.you.know.origin_DNA_origin,Ambiguous.labelling,Common_name_sequenced)%>%
          tally()%>%
          spread(Common_name_sequenced,n,fill ='')
Tab1.orign=dummy%>%
            group_by(report_label,Mismatch_do.you.know.origin_DNA_origin,Ambiguous.labelling,Distribution)%>%
            tally()%>%
            spread(Distribution,n,fill ='')
Tab1.do.u.know.orign=dummy%>%
  group_by(report_label,Mismatch_do.you.know.origin_DNA_origin,Ambiguous.labelling,do_you_know_where_the_fish_comes_from)%>%
  tally()%>%
  spread(do_you_know_where_the_fish_comes_from,n,fill ='')
write.csv(Tab1%>%
            left_join(Tab1.nsp,by=c('report_label','Mismatch_do.you.know.origin_DNA_origin','Ambiguous.labelling'))%>%
            left_join(Tab1.orign,by=c('report_label','Mismatch_do.you.know.origin_DNA_origin','Ambiguous.labelling'))%>%
            left_join(Tab1.do.u.know.orign,by=c('report_label','Mismatch_do.you.know.origin_DNA_origin','Ambiguous.labelling')),
          le.paste("Table_Mismatch.origin_do.you.know.origin.csv"),row.names = F)  
write.csv(dummy%>%
            group_by(report_label,Common_name_sequenced,Mismatch_do.you.know.origin_DNA_origin,Ambiguous.labelling,
                     Distribution,do_you_know_where_the_fish_comes_from)%>%
  tally(),le.paste("Table_Mismatch.origin_do.you.know.origin_supplement.csv"),row.names = F)

Tab1%>%
  ggplot() +
  geom_bar(aes(x=report_label, y=n, fill = Mismatch_do.you.know.origin_DNA_origin),
           stat = "identity")+
  facet_wrap(~Ambiguous.labelling)+
  theme_bw()+
  theme(legend.position = 'top',
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        strip.text.x = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  coord_flip()+xlab('Product label')+ylab('Number of samples')+
  scale_fill_discrete(name = "Mismatched origin",type=Mislabelling.col.vec)
ggsave(le.paste("Mismatch.origin_unambiguous_Do.you.know.origin.tiff"),width = 9,height = 7,compression = "lzw")


#Sankey plot of temporal variation
DATA.resampled=DATA%>%
  filter(Replicate.sample=="YES")%>%
  dplyr::select(shop,post_code,sample_date,report_label,Common_name_sequenced)%>%
  mutate(shop.post.code=paste(shop,post_code))
replicated.shops=sort(unique(DATA.resampled$shop.post.code))
dummy=vector('list',length(replicated.shops))
for(d in 1:length(replicated.shops))
{
  xx=DATA.resampled%>%
              filter(shop.post.code==replicated.shops[d])%>%
              group_by(shop,sample_date)%>%
              mutate(Sampling.event=cur_group_id())%>%
              ungroup()%>%
              mutate(rownumber=row_number())
  dummy[[d]]=xx%>%
              mutate(Triplicate=ifelse(Sampling.event==2 & nrow(xx)>2,'YES','NO'))
  rm(xx)
}
DATA.resampled=do.call(rbind,dummy) 
DATA.resampled=DATA.resampled%>%
                filter(!(shop=='9 oceans fish and chips'))

df <- DATA.resampled%>%filter(Triplicate=='NO' & Sampling.event%in%1:2)%>%
  mutate(report_label=capitalize(report_label),
         Common_name_sequenced=capitalize(Common_name_sequenced))
df1=df%>%filter(Sampling.event==1)%>%dplyr::select(shop,post_code,report_label,Common_name_sequenced)%>%rename(report_label1=report_label,Common_name_sequenced1=Common_name_sequenced)
df2=df%>%filter(Sampling.event==2)%>%dplyr::select(shop,post_code,report_label,Common_name_sequenced)%>%rename(report_label2=report_label,Common_name_sequenced2=Common_name_sequenced)
df=full_join(df1,df2,by=c('shop','post_code'))%>%
  filter(report_label1==report_label2)
p=df%>%
  rename(report_label=report_label1)%>%
  make_long(report_label,Common_name_sequenced1,Common_name_sequenced2)%>%
  mutate(x=ifelse(x=='report_label','Product label',
           ifelse(x=='Common_name_sequenced1','Seq. sp. 1',
           ifelse(x=='Common_name_sequenced2','Seq. sp. 2',
                  NA))))%>%
  ggplot(aes(x = x, 
             next_x = next_x, 
             node = node, 
             next_node = next_node,
             fill = factor(node),
             label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 4, color = 'white') +  
  theme_sankey(base_size = 16) +
  guides(fill = guide_legend(title = "Title"))+
  theme(legend.position = "none")+xlab('')
p+scale_fill_manual(values = species_colors)
ggsave(le.paste("Sankey_label_sequenced_temporal variation.tiff"),width = 6,height = 8,compression = "lzw")

write.csv(df%>%
            dplyr::select(-report_label2)%>%
            rename(report_label=report_label1)%>%
            arrange(report_label,Common_name_sequenced1),
         le.paste("Table_Sankey_label_sequenced_temporal variation.csv"),row.names = F)

#Sankey plot of triplicates variation
df <- DATA.resampled%>%filter(Triplicate=='YES' & Sampling.event==2)%>%
  mutate(report_label=capitalize(report_label),
         Common_name_sequenced=capitalize(Common_name_sequenced))
Replicates.per.shop=table(df$shop)
df = df%>%filter(!shop%in%names(which(Replicates.per.shop<3)))
df2=df%>%filter(rownumber==2)%>%dplyr::select(shop,post_code,report_label,Common_name_sequenced)%>%rename(report_label2=report_label,Common_name_sequenced2=Common_name_sequenced)
df3=df%>%filter(rownumber==3)%>%dplyr::select(shop,post_code,report_label,Common_name_sequenced)%>%rename(report_label3=report_label,Common_name_sequenced3=Common_name_sequenced)
df4=df%>%filter(rownumber==4)%>%dplyr::select(shop,post_code,report_label,Common_name_sequenced)%>%rename(report_label4=report_label,Common_name_sequenced4=Common_name_sequenced)
df=full_join(df2,df3,by=c('shop','post_code'))%>%
  full_join(df4,by=c('shop','post_code'))%>%
  filter(report_label2==report_label3)
p=df%>%
  rename(report_label=report_label2)%>%
  make_long(report_label,Common_name_sequenced2,Common_name_sequenced3,Common_name_sequenced4)%>%
  mutate(x=ifelse(x=='report_label','Product label',
           ifelse(x=='Common_name_sequenced2','Seq. sp. rep. 1',
           ifelse(x=='Common_name_sequenced3','Seq. sp. rep. 2',
           ifelse(x=='Common_name_sequenced4','Seq. sp. rep. 3',
           NA)))))%>%
  ggplot(aes(x = x, 
             next_x = next_x, 
             node = node, 
             next_node = next_node,
             fill = factor(node),
             label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 4, color = 'white') +  
  theme_sankey(base_size = 16) +
  guides(fill = guide_legend(title = "Title"))+
  theme(legend.position = "none")+xlab('')
p+scale_fill_manual(values = species_colors)
ggsave(le.paste("Sankey_label_sequenced_triplication.tiff"),width = 6,height = 8,compression = "lzw")
write.csv(df%>%
            dplyr::select(-c(report_label3,report_label4))%>%
            rename(report_label=report_label2)%>%
            arrange(report_label,Common_name_sequenced2),
          le.paste("Table_Sankey_label_sequenced_triplication.csv"),row.names = F)



#Raw table
Raw.table=DATA%>%
  dplyr::select(sample_date,shop,post_code,area,report_label,Common_name_sequenced,Scientific_name_sequenced)%>%
  group_by(shop,post_code,area,sample_date)%>%
  mutate(Sampling.event=cur_group_id())%>%
  ungroup()%>%
  arrange(shop,post_code,area,sample_date)
write.csv(Raw.table,le.paste("Table_raw.csv"),row.names = F)



#Table advertised origin
dummy=DATA%>%
  filter(!is.na(advertised_product_origin))%>%
  filter(!Distribution=='Global')%>%  #Mismatch_origin_DNA_origin
  mutate(Ambiguous.labelling=ifelse(Ambiguous.labelling=='Yes','Ambiguous','Unambiguous'),
         report_label=capitalize(report_label))
Tab1=dummy%>%
  group_by(report_label,Mismatch_label_DNA_origin,Ambiguous.labelling)%>%
  tally()
Tab1.nsp=dummy%>%
  group_by(report_label,Mismatch_label_DNA_origin,Ambiguous.labelling,Common_name_sequenced)%>%
  tally()%>%
  spread(Common_name_sequenced,n,fill ='')
Tab1.orign=dummy%>%
  group_by(report_label,Mismatch_label_DNA_origin,Ambiguous.labelling,Distribution)%>%
  tally()%>%
  spread(Distribution,n,fill ='')
Tab1.do.u.know.orign=dummy%>%
  group_by(report_label,Mismatch_label_DNA_origin,Ambiguous.labelling,do_you_know_where_the_fish_comes_from)%>%
  tally()%>%
  spread(do_you_know_where_the_fish_comes_from,n,fill ='')
write.csv(Tab1%>%
            left_join(Tab1.nsp,by=c('report_label','Mismatch_label_DNA_origin','Ambiguous.labelling'))%>%
            left_join(Tab1.orign,by=c('report_label','Mismatch_label_DNA_origin','Ambiguous.labelling'))%>%
            left_join(Tab1.do.u.know.orign,by=c('report_label','Mismatch_label_DNA_origin','Ambiguous.labelling')),
          le.paste("Table_Mismatch.advertised_origin.csv"),row.names = F)  

write.csv(dummy%>%dplyr::select(report_label,advertised_product_origin,Common_name_sequenced,Distribution,
                                Mismatch_label_DNA_origin)%>%arrange(report_label,Common_name_sequenced),
          le.paste("Table_Mismatch.advertised_origin_raw.csv"),row.names = F)
