library(httr)
library(jsonlite)
library(tibble)


####################################################
## Local IPFS Daemon Node
####################################################
## Add a File on Local Node and Pin it
post_local_pin <- function(filepath,ipfsip)
{
	Sys.sleep(2)
	req <- POST(
				paste0(ipfsip,"/api/v0/add"),
				body = list(
							file = upload_file(filepath)
						)
			)
	if(status_code(req)==200)
	{
		return(content(req)$Hash)
	}
	return(paste0("Error:",req$status_code,";",content(req)$error$details))
}


## Read an Object from local node
read_local_pin <- function(ipfs_cid,ipfsip)
{
	Sys.sleep(2)
	req <- POST(paste0(ipfsip,"/api/v0/cat?arg=",ipfs_cid))
	if(status_code(req)==200)
	{
		return(as_tibble(read.csv(text=content(req),check.names=FALSE)))
	}
	return(paste0("Error:",req$status_code,";",content(req)$error$details))
}

## Local Node Status
read_local_status <- function(ipfsip)
{
	req <- tryCatch(
	{
		POST(paste0(ipfsip,"/api/v0/version"))
	},
	error = function(err)
	{
		return(FALSE)
	}
	)
	if(class(req)=="response")
	{
		return(TRUE)
	}
	return(FALSE)
}
####################################################
####################################################


####################################################
## Pinata Pinning Service
####################################################
## Popular Gateway List from https://ipfs.github.io/public-gateway-checker/
gateway_list <- c("ipfs.io","dweb.link","cf-ipfs.com","ipfs-gateway.cloud","nftstorage.link","4everland.io","infura-ipfs.io")

## Pinata API Status
read_pinata_status <- function(JWT)
{
	req <- GET("https://api.pinata.cloud/data/testAuthentication",add_headers("Authorization" = paste0("Bearer ",JWT)))
	if(status_code(req)==200) return(TRUE)
	return(FALSE)
}

## Pin a file on Pinata
post_pinata_pin <- function(filepath,JWT)
{
	req <- POST(
				"https://api.pinata.cloud/pinning/pinFileToIPFS",
				body = list(
							pinataOptions = '{"cidVersion": 0}',
							pinataMetadata = paste0('{"name": "',basename(filepath),'"}'),
							file = upload_file(filepath)
						),
				add_headers("Authorization" = paste0("Bearer ",JWT))
			)
	if(status_code(req)==200)
	{
		if("isDuplicate" %in% names(content(req))) message("Already Pinned Returning CID")
		return(content(req)$IpfsHash)
	}
	return(paste0("Error:",req$status_code,";",content(req)$error$details))
}


## Read file using CID and a public ipfs.io gateway
read_pinata_pin <- function(ipfs_cid,gateway)
{
	req <- tryCatch(
	{
		GET(paste0("https://",gateway,"/ipfs/",ipfs_cid),timeout(10))
	},
	error = function(err)
	{
		if(grepl("Operation timed out",err$message)) return(paste0("Error:","Timed Out"))
		return("Error:Unknown Error")
	}
	)
	if(class(req)=="response")
	{
		if(status_code(req)==200) return(content(req))
		if(status_code(req)==504) return(paste0("Error:",req$status_code,";Timed Out"))
		return(paste0("Error:",req$status_code,";",content(req)$error$details))
	}
	return(req)
}


## Remove the pinned file from Pinata using CID
remove_pinata_pin <- function(ipfs_cid,JWT)
{
	req <- DELETE(
				paste0("https://api.pinata.cloud/pinning/unpin/",ipfs_cid),
				add_headers("Authorization" = paste0("Bearer ",JWT))
			)
	if(status_code(req)==200) return("Removed Pinned Item")
	return(paste0("Error:",req$status_code,";",content(req)$error$details))
}
####################################################
####################################################

