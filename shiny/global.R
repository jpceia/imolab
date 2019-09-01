
dataFolder <- "C:/Users/joaop/OneDrive - insidemedia.net/Data/1 - Houses"
district_meta <- read_csv(
  file.path(dataFolder, "geodata", "district_meta.csv"),
  locale = readr::locale(encoding = "latin1")
  ) %>%
  filter(Continente) %>%
  select(Dicofre, Designacao)

city_meta <- read_csv(
  file.path(dataFolder, "geodata", "district_meta.csv"),
  locale = readr::locale(encoding = "latin1")
) %>%
  mutate(code1 = as.integer(Dicofre / 100)) %>%
  filter(code1 %in% district_meta$Dicofre) %>%
  select(code1, Dicofre, Designacao)


district_list <- district_meta$Dicofre
names(district_list) <- district_meta$Designacao
district_list[' '] <- 0  # all


districts <- list(
  ' '='ALL',
  'Aveiro'='aveiro',
  'Beja'='beja',
  'Braga'='braga',
  'Braganca'='braganca',
  'Castelo Branco'='castelo branco',
  'Coimbra'='coimbra',
  'Evora'='evora',
  'Faro'='faro',
  'Guarda'='guarda',
  'Leiria'='leiria',
  'Lisboa'='lisboa',
  'Portalegre'='portalegre',
  'Porto'='porto',
  'Santarem'='santarem',
  'Setubal'='setubal',
  'Viana do Castelo'='viana do castelo',
  'Vila Real'='vila real',
  'Viseu'='viseu'
)





prop_types <- c(
  'Apartment',
  'House',
  'Terrain',
  'Store',
  'Warehouse',
  'Garage',
  'Office',
  'Building',
  'Farm'
)