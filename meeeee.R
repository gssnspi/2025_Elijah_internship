

ahies=read_csv("C:/Users/Dell/Desktop/AHIES/AHIES2022Q1_2023Q3_SEC01234_202402.csv")
getwd()



employed_by_sex = ahies %>% 
  select(econact, s1aq1,quarter, s1aq4y,pop_weight) %>% 
  filter(econact=="Employed", s1aq4y>=15) %>% 
  group_by(quarter, s1aq1) %>% 
  summarize(total_employed = sum(pop_weight),
            .groups = "drop")

ggplot(employed_by_sex ,aes(x=quarter, y=total_employed, fill = s1aq1))+
  geom_col()+
  geom_text(aes(label = round(total_employed/1000000,2)), 
            position = position_stack(vjust = 0.5), 
            color = "black", size =3.0) +
  labs(x=NULL, y="Millions", fill=NULL)+
  scale_fill_manual(values = c("Female"="#F66068", "Male"="#206095"))



  
employed_by_locality = ahies %>% 
  select(econact,quarter,urbrur,s1aq4y,pop_weight) %>% 
  filter(econact=="Employed", s1aq4y>=15) %>% 
  group_by(quarter,urbrur) %>% 
  summarize(employed_by_locality = sum(pop_weight),
            .groups = "drop")

ggplot(employed_by_locality, aes(x=quarter, y=employed_by_locality, fill = urbrur))+
  geom_col()+
  labs(x=NULL , y="Millions", fill=NULL)+
  scale_fill_manual(values = c("Rural"="#22D0B6","Urbanb"="#871A5B"))


unemployment_by_sex = ahies%>%
  select(econact, s1aq1, pop_weight,quarter) %>% 
  filter(econact=="Unemployed") %>% 
  group_by(quarter, s1aq1) %>% 
  summarize(unemployment_by_sex=sum(pop_weight),
            .groups = "drop")

merged_data = unemployment_by_sex %>% 
  left_join(employed_by_sex, by = c("quarter","s1aq1")) %>% 
  mutate(
    labor_force = unemployment_by_sex + total_employed,
    unemployment_rate = (unemployment_by_sex / labor_force) * 100
  )

overall_unemployment_by_both_sex = merged_data %>% 
  group_by(quarter) %>% 
  summarise(
    unemployment_by_sex=sum(unemployment_by_sex),
    total_employed=sum(total_employed),
    labor_force=total_employed+unemployment_by_sex,
    overall_unemployment_by_both_sex= (unemployment_by_sex/labor_force)*100
    
  )



bind_rows(
merged_data %>%  
  select(quarter, s1aq1, unemployment_rate),

overall_unemployment_by_both_sex %>% 
  select(quarter, unemployment_rate=overall_unemployment_by_both_sex) %>% 
  mutate(s1aq1="Both Sexes")
) %>% 
  ggplot( aes(x=quarter, y=unemployment_rate, group = s1aq1, colour =s1aq1))+
  geom_line()+
  scale_colour_manual(values = c("Female"="#F66068", "Male"="#206095", 
                                 "Both Sexes"="#27A0CC"))+
  labs(x=NULL, y="Percent", fill=NULL)+
  theme_minimal()
  
  
unemployment_by_locality=ahies %>% 
  select(econact,urbrur,quarter,pop_weight) %>% 
  filter(econact=="Unemployed") %>% 
  group_by(quarter,urbrur) %>% 
  summarise(total_unemployment= sum(pop_weight),
            .groups = "drop")


employed_by_locality = ahies %>% 
  select(econact,quarter,urbrur,s1aq4y,pop_weight) %>% 
  filter(econact=="Employed", s1aq4y>=15) %>% 
  group_by(quarter,urbrur) %>% 
  summarize(employed_by_locality = sum(pop_weight),
            .groups = "drop")

merged_data1= unemployment_by_locality %>% 
  left_join(employed_by_locality, by= c("quarter", "urbrur"))%>%
  mutate(
    labor_force= total_unemployment+employed_by_locality,
    unemployment_rate=(total_unemployment/labor_force)*100
  )

ggplot(merged_data1, aes(x=quarter, y=unemployment_rate,group = urbrur, colour = urbrur))+
  geom_line()+
  scale_colour_manual(values = c("Rural"="#22D0B6","Urbanb"="#871A5B"))


unemployment_rate=merged_data1 %>% 
  group_by(quarter) %>% 
  summarise(
    total_unemployment=sum(total_unemployment),
    total_employed=sum(employed_by_locality),
    labor_force=total_unemployment + total_employed,
    unemployment_rate=(total_unemployment / labor_force)*100,
    .groups = "drop"
  )

bind_rows(
  merged_data1 %>% 
    select(quarter, urbrur, unemployment_rate),
  
  unemployment_rate %>% 
    select(quarter,unemployment_rate) %>% 
    mutate(urbrur="All Localities")
  ) %>% 
  ggplot(aes(x=quarter, y=unemployment_rate, group = urbrur, colour = urbrur))+
  geom_line(size=1.2)+
  scale_colour_manual(values = c("Rural"="#22D0B6","Urbanb"="#871A5B",
                                 "All Localities"="#27A0CC"))+
  labs(x=NULL, y="Percent", fill=NULL)+
  theme_minimal()+
  theme(legend.positon="top")




unemployment_by_youth=ahies %>% 
  select(econact,pop_weight,quarter,s1aq4y) %>% 
  filter(econact=="Unemployed", between(s1aq4y,15 , 35)) %>% 
  group_by(quarter) %>% 
  summarise(unemployment_by_youth=sum(pop_weight),
            .groups = "drop")


unemployment_by_youth1=ahies %>% 
  select(econact,pop_weight,quarter,s1aq4y) %>% 
  filter(econact=="Unemployed", between(s1aq4y,15 , 24)) %>% 
  group_by(quarter) %>% 
  summarise(unemployment_by_youth1=sum(pop_weight),
            .groups = "drop")

employed_by_youth1=ahies %>% 
  select(econact,pop_weight,quarter,s1aq4y) %>% 
  filter(econact=="Employed", between(s1aq4y,15 , 24)) %>%
  group_by(quarter) %>% 
  summarise(employed_by_youth1=sum(pop_weight),
            .groups = "drop")

merged_data4= unemployment_by_youth1 %>% 
  left_join(employed_by_youth1, by= c("quarter"))%>%
  mutate(
    labor_force1= unemployment_by_youth1+employed_by_youth1,
    unemployment_rate_youth=(unemployment_by_youth1/labor_force1)*100
  )

ggplot() +
  
  geom_point(data = merged_data4, aes(x = quarter, y = unemployment_rate_youth)) +
  geom_smooth(data = merged_data4, aes(x = quarter, y = unemployment_rate_youth, group = "quarter"),
              se = F) + 
  geom_text(data = merged_data4, aes(x = quarter, y = unemployment_rate_youth, label = round(unemployment_rate_youth, 1))) +
  scale_y_continuous(limits = c(10, 35))+

  
  
  geom_point(data = merged_data3, aes(x = quarter, y = unemployment_rate_youth)) +
  geom_smooth(data = merged_data3, aes(x = quarter, y = unemployment_rate_youth, group = "quarter"),
              se = F) + 
  geom_text(data = merged_data3, aes(x = quarter, y = unemployment_rate_youth, label = round(unemployment_rate_youth, 1))) +
  scale_y_continuous(limits = c(10, 35))
  
  
  
  
   
  
  
  
  geom_smooth(data = merged_data4,
              aes(x = quarter, y = unemployment_rate_youth),
              color = "#3333FF", se = FALSE) +
  labs(x = "Quarter", y = NULL, colour=NULL) +
  theme_minimal()

  



ggplot(unemployment_by_youth, aes(x=quarter, y=unemployment_by_youth, fill = unemployment_by_youth))+
  geom_bar(stat = "identity", fill="#FF3333")+
  geom_text(aes(label = comma(unemployment_by_youth)), 
            vjust = 1.5, fontface="bold", color = "white", size = 4)+
  labs(x=NULL, y="Number of Persons", fill=NULL)

  
           
employed_by_youth=ahies %>% 
  select(econact,pop_weight,quarter,s1aq4y) %>% 
  filter(econact=="Employed", between(s1aq4y,15 , 35)) %>%
  group_by(quarter) %>% 
  summarise(employed_by_youth=sum(pop_weight),
            .groups = "drop")
  

merged_data3= unemployment_by_youth %>% 
  left_join(employed_by_youth, by= c("quarter"))%>%
  mutate(
    labor_force= unemployment_by_youth+employed_by_youth,
    unemployment_rate_youth=(unemployment_by_youth/labor_force)*100
  )

geom_smooth(data = merged_data4,
            aes(x = quarter, y = unemployment_rate_youth),
            color = "#3333FF", se = FALSE, method = "loess") +
  labs(x = "Quarter", y = NULL) +
  theme_minimal()





regional_shp <- read_sf("C:/Users/Dell/Desktop/AHIES/gss_phc2021_bnd_edgematched_20240821151831.gpkg", 
                        layer = "gss_phc2021_16_regions")




district_shp <- read_sf("C:/Users/Dell/Desktop/AHIES/gss_phc2021_bnd_edgematched_20240821151831.gpkg", 
                        layer = "gss_phc2021_261_admin_districts")


employed_by_youth1 <- employed_by_youth1 |> 
  left_join(district_shp,
            by = c("District"="distcode_name")) |> 
  st_as_sf()


fig42_sf |> 
  ggplot()+
  geom_sf(aes(fill=Percent, geometry = geom), colour = "#e5e5e5", linewidth=0.01)+
  geom_sf(data = regional_shp, aes(geometry = geom),colour="black",linewidth=0.1,
          fill = "transparent") +
  # geom_sf_text(
  #   aes(label = case_when(
  #     str_detect(district, "(?i)\\bMunicipal\\b") ~ str_wrap(str_remove(district,"(?i)\\bMunicipal\\b"),width = 15),
  #     TRUE ~ str_wrap(district,width = 15)
  #   )), size = 1)+
  geom_sf_text(
    data = regional_shp,
    aes(label = region_name, geometry = geom, colour = ifelse(region_name == "Greater Accra","yes","no")),
    vjust = .5, show.legend = F,
    family = "Century Gothic", size = 3, fontface = "bold") +
  # scale_fill_gradientn(colours =(population_color_scheme), name = "Percent",
  #                      limits = c(10, 80)) +
  scale_fill_gradientn(colours =(incidence_color_scheme), name = "Percent",
                       limits = c(10, 80)) +
  scale_color_manual(values = c("black","black"))+
  labs(x=NULL,y=NULL)+
  theme(legend.text  = element_text(size = 8.5, family = "Century Gothic", face = "bold"),
        legend.title = element_text(size = 8.5, family = "Century Gothic", face="bold"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.95,0.6),
        plot.title = element_text(family = "Century Gothic", hjust = 0.3, face = "bold"))


glss<- data1 |>
  select(s1q5y,sex,region,s1q6,WTA_S) |>
  filter(s1q5y>=12) |>
  mutate(across(everything(), as_factor),
         WTA_S = as.numeric(WTA_S)) |>
  filter(sex=="female")
group_by(region, s1q6) |>
  summarize(male=sum(WTA_S),
            .groups = "drop")

gls <- data1 %>%
  select(s1q2, s1q5y, region, s1q6, WTA_S) |>
  # filter(sex == 1) |>
  filter(s1q5y >= 12, !is.na(s1q5y)) |>
  mutate(across(c(s1q2, s1q5y, region, s1q6), as_factor)) |>
  mutate(WTA_S = as.numeric(WTA_S)) |>                        
  group_by(region,s1q2,s1q6) |>
  summarize(total = sum(WTA_S), .groups = "drop")|>
  pivot_wider(names_from = s1q2, values_from = total)|>
  # rowwise() %>% 
  # mutate(both_sex=sum(c(Male,Female)))
  mutate(Both_Sex = rowSums(across(c(Male,Female))))|>
  pivot_longer(cols = c(Male, Female, Both_Sex), 
               names_to = "sex", values_to = "total"  )|>
  pivot_wider(names_from = s1q6, values_from = total)|>
  mutate(Total= rowSums(across(-c(region,sex,Married))))|>
  pivot_longer(cols = -c(region,sex))|>
  pivot_wider(names_from = region, values_from = value)|>
  mutate(Total= rowSums(across(-c(sex,name))))|>
  filter(sex== "Both_Sex")

# mean(is.na(gls))
data1<- read_dta("C:/Users/Dell/Desktop/Sly/File 1.dta")
View()

gls <- data1%>%
  select(s1q5y, region, s1q2, s1q6, WTA_S) %>%
  mutate(across(!c(s1q5y, WTA_S), as_factor)) %>%
  filter(s1q5y >= 12, !is.na(s1q6)) %>%
  group_by(region, s1q6, s1q2) %>%
  summarise(pop = sum(WTA_S), .groups = "drop")  %>% 
  pivot_wider(names_from = s1q2, values_from = pop) %>%
  mutate(both_sex = rowSums(across(c(Male, Female)))) %>% 
  pivot_longer(cols = -c(region, s1q6), names_to = "sex", values_to = "total") %>% 
  pivot_wider(names_from = s1q6, values_from = total) %>%
  mutate(total = rowSums(across(-c(region, sex)))) %>% 
  pivot_longer(cols = -c(region, sex), names_to = "marital_status", values_to = "all_status") %>% 
  #filter(sex == "both_sex") %>% 
  pivot_wider(names_from = region, values_from = all_status)|>
  mutate(Total= rowSums(across(-c(sex,marital_status))))|>
  pivot_longer(cols= -c(sex , marital_status), names_to = "region", values_to ="Total")|>
  select(region,sex,marital_status,Total)|>
  group_by(region,sex)|>
  mutate(percent = round(Total/Total[marital_status == "total"]*100,1))
View(gls)












