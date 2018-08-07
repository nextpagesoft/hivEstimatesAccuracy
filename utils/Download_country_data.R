conn <- url("https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv")
countryData <- read.csv(conn, stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
data.table::setDT(countryData)

countryData <- countryData[, .(
  official_name_en,
  `ISO3166.1.Alpha.2`,
  `UNTERM.English.Formal`,
  Continent,
  `Region.Name`,
  `Sub.region.Name`
)]

data.table::setnames(
  countryData,
  c("Name", "Code", "FormalName", "ContinentCode", "RegionName", "SubRegionName")
)

countryData <- countryData[!is.na(Code)]

countryData[Code == "TW", ":="(
  Name = "Taiwan",
  FormalName = "Republic of China",
  RegionName = "Asia",
  SubRegionName = "Eastern Asia"
)]

countryData[Code == "GR", ":="(
  Code = "EL"
)]

countryData[Code == "GB", ":="(
  Code = "UK"
)]

countryData[, TESSyCode := NA_character_]
countryData[
  Code %chin% c('AU', 'CC', 'CX', 'HM', 'NF', 'NZ'),
  TESSyCode := "AUSTNZ"
]
countryData[
  Code %chin% c('AG', 'AI', 'AW', 'BB', 'BL', 'BM', 'BQ', 'BS', 'BV', 'CU', 'CW', 'DM', 'DO', 'GD',
                'GP', 'HT', 'JM', 'KN', 'KY', 'LC', 'MF', 'MQ', 'MS', 'PR', 'SX', 'TC', 'TT', 'VC',
                'VG', 'VI'),
  TESSyCode := "CAR"
]
countryData[
  Code %chin% c('AL', 'BA', 'BG', 'CY', 'CZ', 'HR', 'HU', 'ME', 'MK', 'PL', 'RO', 'RS', 'SI', 'SK',
                'TR'),
  TESSyCode := "CENTEUR"
]
countryData[
  Code %chin% c('AS', 'CK', 'CN', 'FJ', 'HK', 'JP', 'KP', 'KR', 'MN', 'MO', 'NU', 'PF', 'PG', 'SB',
                'TK', 'TO', 'TV', 'TW', 'VU', 'WF', 'WS'),
  TESSyCode := "EASTASIAPAC"
]
countryData[
  Code %chin% c('AM', 'AZ', 'BY', 'EE', 'GE', 'KG', 'KZ', 'LT', 'LV', 'MD', 'RU', 'TJ', 'TM', 'UA',
                'UZ'),
  TESSyCode := "EASTEUR"
]
countryData[
  Code %chin% c('AR', 'BO', 'BR', 'BZ', 'CL', 'CO', 'CR', 'EC', 'FK', 'GF', 'GS', 'GT', 'GY', 'HN',
                'MX', 'NI', 'PA', 'PE', 'PY', 'SR', 'SV', 'UY', 'VE'),
  TESSyCode := "LATAM"
]
countryData[
  Code %chin% c('AE', 'BH', 'DZ', 'EG', 'EH', 'IQ', 'JO', 'KW', 'LB', 'LY', 'MA', 'OM', 'PS', 'QA',
                'SA', 'SD', 'SS', 'SY', 'TN', 'YE'),
  TESSyCode := "NORTHAFRMIDEAST"
]
countryData[
  Code %chin% c('CA', 'PM', 'US'),
  TESSyCode := "NORTHAM"
]
countryData[
  Code %chin% c('AF', 'BD', 'BN', 'BT', 'FM', 'GU', 'ID', 'IN', 'IR', 'KH', 'KI', 'LA', 'LK', 'MH',
                'MM', 'MP', 'MV', 'MY', 'NC', 'NP', 'NR', 'PH', 'PK', 'PN', 'PW', 'SG', 'TH', 'TL',
                'UM', 'VN'),
  TESSyCode := "SOUTHASIA"
]
countryData[
  Code %chin% c('AO', 'BF', 'BI', 'BJ', 'BW', 'CD', 'CF', 'CG', 'CI', 'CM', 'CV', 'DJ', 'ER', 'ET',
                'GA', 'GH', 'GM', 'GN', 'GQ', 'GW', 'IO', 'KE', 'KM', 'LR', 'LS', 'MG', 'ML', 'MR',
                'MU', 'MW', 'MZ', 'NA', 'NE', 'NG', 'RE', 'RW', 'SC', 'SH', 'SL', 'SN', 'SO', 'ST',
                'SZ', 'TD', 'TF', 'TG', 'TZ', 'UG', 'YT', 'ZA', 'ZM', 'ZW'),
  TESSyCode := "SUBAFR"
]
countryData[
  Code %chin% c('AQ'),
  TESSyCode := "UNK"
]
countryData[
  Code %chin% c('AD', 'AT', 'AX', 'BE', 'CH', 'DE', 'DK', 'EL', 'ES', 'FI', 'FO', 'FR', 'GG', 'GI',
                'GL', 'IE', 'IL', 'IM', 'IS', 'IT', 'JE', 'LI', 'LU', 'MC', 'MT', 'NL', 'NO', 'PT',
                'SE', 'SJ', 'SM', 'UK', 'VA'),
  TESSyCode := "WESTEUR"
]

save(countryData,
     file = "D:/_REPOSITORIES/Github/hivEstimatesAccuracy/data/countryData.rda",
     compress = "xz")
