
# GetFREData

`GetFREData` is an R package for downloading information about companies traded at B3, the Brazilian financial exchange. The source of the data is [CVM's ftp site](http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/FRE/) and B3's [FRE system](http://www.b3.com.br/pt_br/produtos-e-servicos/negociacao/renda-variavel/acoes/consultas/informacoes-por-periodo/), the official registration of all major corporate actions. 

The data available in FRE is a rich and very useful for **corporate finance studies**. See an example  [here](http://bvmf.bmfbovespa.com.br/cias-listadas/empresas-listadas/ResumoDemonstrativosFinanceiros.aspx?codigoCvm=9512&idioma=pt-br). The `GetFREData` currently includes the following tables, starting from 2010:

- List of stockholders
- All capital issues 
- Stock value over years
- Compensation of boards and directors
- Composition of boards and comittes
- Family relations within the company
- List of companies related to family members
- Stock details
- Intangible details
- Auditing details
- Dividends details

Annualy compiled datasets from FRE for all available companies since 2010 are available at [https://www.msperlin.com/blog/data/](https://www.msperlin.com/blog/data/).

## Installation

```
# CRAN (stable)
install.packages('GetFREData')

# github (development)
if (!require(devtools)) install.packages('devtools')
if (!require(GetFREData)) devtools::install_github('msperlin/GetFREData') 
```

## Example of usage

```
library(GetFREData)
library(tidyverse)

search_company('grendene', 
               cache_folder = tempdir())

l_fre <- get_fre_data(companies_cvm_codes = 19615,
                      fre_to_read = 'last',
                      first_year = 2020,
                      last_year = 2020, 
                      cache_folder = tempdir())
                      
glimpse(l_fre)
```
