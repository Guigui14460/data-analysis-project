# Data Analysis Project

University project proposed in the subject "Data Analysis" in 1st year of MSc of Computer Science in Imaging & Machine Learning at the University of Caen Normandy in order to make us use the tools seen in class like descriptive statistics, ICA (Independent Component Analysis), PCA (Principal Component Analysis) and MCA (Multiple Correspondance Analysis).

## Table of contents

  - [Table of contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Setup](#setup)
  - [Commands](#commands)
  - [Authors](#authors)
  - [License](#license)

## Introduction
The goal of the project is to realize an analysis on data from different sensors distributed on the Touques river in Normandy.

## Setup
You need to have R or Rstudio installed on your machine to be able to use this project.

If you want to compile the report, we recommand to install [pandoc](https://pandoc.org/index.html) and [MiKTeX](https://miktex.org/download).

## Commands
- To run the script :
```shell
$ Rscript --encoding=UTF-8 analysis.r
```
This command will generate graphics in the `graphics` folder.

- To build the PDF report :
```shell
$ Rscript compile_report.r
```

## Authors
- [BOCAGE Arthur](https://github.com/Turlututur)
- [PIERRE Corentin](https://github.com/coco-ia)
- [PIGNARD Alexandre](https://github.com/Myrani)
- [LETELLIER Guillaume](https://github.com/Guigui14460)

## License
Project under the MIT license.
