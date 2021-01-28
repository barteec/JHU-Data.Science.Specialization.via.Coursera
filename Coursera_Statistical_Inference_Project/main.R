library(knitr)

knit("Part2_tooth_growth_inferential_Project.Rmd")

rmarkdown::render(input = "Part2_tooth_growth_inferential_Project.Rmd", 
                  output_format = "pdf_document", output_file = "Part2_tooth_growth_inferential_Project.pdf")


