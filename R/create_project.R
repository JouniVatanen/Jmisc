#' Create project structure
#'
#' @description: This function helps you create a project structure. This includes:
#' \itemize{
#'   \item R (folder)
#'   \item data (folder)
#'   \item doc (folder)
#'   \item output (folder)
#'   \item posted (folder)
#'   \item work (folder)
#'   \item .Rprofile
#'   \item .Rproj (optional; created with \code{\link{createProject}})
#' }
#' @param dir character: Directory where the file structure is created, absolute
#' or relative to the current working directory. The current working directory
#' by default.
#' @param notebook logical: Create notebook file.
#' @param project logical: Create Rproject file.
#' @keywords project skeleton
#' @export
#' @examples
#' create_project()
#' @export

create_project <- function(dir = '.', notebook = TRUE, project = TRUE) {

    if (dir != '.') {
    if (dir.exists(dir)) {
      message('Directory already exists and will be used.')
    } else {
      message(paste0("Creating '", dir, "/'"))
      dir.create(dir)
    }
  }

  dir <- addBackslash(dir)

  folders <- c('data', 'report', 'R', 'work', 'doc', 'posted')
  message(paste0('Creating directories: ', paste0(folders, collapse = ', ')))
  lapply(paste0(dir, folders), dir.create)

  if (project) {
    message("Creating R Project")
    create_proj(dir)
  }

  if (notebook) {
    message('Creating R Notebook')
    create_notebook(dir)
  }
}

#' Create R project
#'
#' @description Creates an R project with useful configuration. If the project
#' contains a package, the respective options are set. The project is named
#' after the folder which contains it.
#' Used in \code{\link{createProjectSkeleton}}.
#' @param dir character: Directory where the R project is created; current
#' working directory by default
#'
#' @examples
#' \dontrun{
#' createProject(pkg = FALSE, dir = "./")
#' dir.create("tmp")
#' createProject(pkg = TRUE, pkgOnToplevel = FALSE, dir = "tmp")}
#'
#' @export
#'
create_proj <- function(dir = '.') {

  prefs <- c("Version: 1.0",
             "",
             "RestoreWorkspace: No",
             "SaveWorkspace: No",
             "AlwaysSaveHistory: No",
             "",
             "EnableCodeIndexing: Yes",
             "UseSpacesForTab: Yes",
             "NumSpacesForTab: 2",
             "Encoding: UTF-8",
             "",
             "RnwWeave: Sweave",
             "LaTeX: pdfLaTeX")

  dir <- addBackslash(dir)

  projName <- (if (dir == "./") getwd() else dir) %>%
    strsplit("/") %>% unlist %>% `[`(length(.))

  writeLines(text = prefs,
             con = paste0(dir, projName, ".Rproj"))
}

#' Create R project
#'
#' @description Creates an R project with useful configuration. If the project
#' contains a package, the respective options are set. The project is named
#' after the folder which contains it.
#' Used in \code{\link{createProjectSkeleton}}.
#' @param dir character: Directory where the R project is created; current
#' working directory by default
#'
#' @examples
#' \dontrun{
#' createProject(pkg = FALSE, dir = "./")
#' dir.create("tmp")
#' createProject(pkg = TRUE, pkgOnToplevel = FALSE, dir = "tmp")}
#'
#' @export
#'
create_notebook <- function(dir = '.') {

  prefs <- c("---",
             "title: 'xxx'",
             "author: 'Jouni Vatanen'",
             "date: 'x.x.2018'",
             "output:",
             "  html_document:",
             "  toc: TRUE",
             "toc_float: TRUE",
             "df_print: paged",
             "code_folding: hide",
             "number_sections: TRUE",
             "powerpoint_presentation:",
             "  reference_doc: 'C://Users//vatanjj//OneDrive - Keskinäinen Eläkevakuutusyhtiö IImarinen//8-Management//Documents//Malli - Powerpoint-esitysv2.pptx'",
             "html_notebook:",
             "  code_folding: hide",
             "word_document:",
             "  reference_docx: 'C://Users//vatanjj//OneDrive - Keskinäinen Eläkevakuutusyhtiö IImarinen//8-Management//Documents//Malli - Word.docx'",
             "editor_options:",
             "  chunk_output_type: console",
             "---",
             "",
             "<!--",
             "Lyhyt ohje",
             "1.",
             "2.",
             "3.",
             "-->",
             "",
             "```{r setup, echo = FALSE, results = 'hide', warning = FALSE, error = FALSE, message = FALSE}",
             "# Define paths",
             "",
             "# Define variables")

  dir <- addBackslash(dir)

  projName <- (if (dir == "./") getwd() else dir) %>%
    strsplit("/") %>% unlist %>% `[`(length(.))

  writeLines(text = prefs,
             con = paste0(dir, projName, ".Rproj"))
}

