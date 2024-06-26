% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{dump_fasta}
\alias{dump_fasta}
\title{Dumpto FASTA file(s)}
\usage{
dump_fasta(x, outpath = ".", separate = FALSE, filename = "dump.fasta.gz")
}
\arguments{
\item{x}{table of NCBI search results with Species and Fasta columns or a named list of fastas}

\item{outpath}{chr, output path to save the file(s)}

\item{separate}{logical, if TRUE save one file per species otherwise save all to one file}

\item{filename}{chr, if separate is FALSE then write to this filename, otherwise
write to files named in the pattern Species.fasta.  If a single file then please use the .gz extension.}
}
\value{
the input table (unchanged)
}
\description{
Dumpto FASTA file(s)
}
