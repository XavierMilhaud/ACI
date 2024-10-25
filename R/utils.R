
utils::globalVariables(c("Country"))

aciStartupMessage <- function()
{
  msg <- c(paste0(
    "This is package ACI, version ",
    utils::packageVersion("ACI")),
    "\n-------------------------------\n",
    "Type 'citation(\"ACI\")' for citing this R package in publications.",
    "\n-------------------------------\n",
    "This work was partly conducted within the Research Chair DIALog under the aegis of the Risk Foundation, an initiative by CNP Assurances.\n")
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # unlock .aci variable allowing its modification
  #unlockBinding(".ACI", asNamespace("ACI"))
  # startup message
  msg <- aciStartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'ACI' version", packageVersion("ACI"))
  base::packageStartupMessage(msg)
  base::invisible()
}
