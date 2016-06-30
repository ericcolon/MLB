library(rsconnect)
rsconnect::setAccountInfo(name = 'wesleypasfield', 
                          token = '2732D4C57528298E4546CF9DB272F2CF', 
                          secret = 'oM9XSnYYw1fOpl5OzpI0uVFDLNAj550v2N+EIeJX')
deployApp(appName = 'dfsPicks')
