#' Validate address with U.S. Postal Service (USPS) Web Tools
#'
#' @param address_1 Optional. Secondary Delivery Address. May contain secondary unit designator, such as APT or SUITE.
#' @param address_2 Optional. Primary Delivery Address.
#' @param city Optional. City name of the destination address. Maximum characters allowed: 15.
#' @param state Optional. Two-character state code of the destination address. Maximum characters allowed: 2.
#' @param zip5 Optional. Destination 5-digit ZIP Code.StringMust be 5-digits. Numeric values (0-9) only.
#' @param zip4 Optional. Destination 4-digit +ZIP Code. Numeric values (0-9) only.
#' @param full_info Required. Logical value used to flag return of all response fields.
#' @return list
#' @export
valid_usps_address <- function(address_1 = "",
                                 address_2 = "1100 Wilson Blvd",
                                 city = "Arlington",
                                 state = "VA",
                                 zip5 = "",
                                 zip4 = "",
                                 full_info = TRUE,
                                 usps_userid = Sys.getenv("USPS_WEBAPI_USERNAME")) {
  if (full_info == TRUE) rev <- 1 else rev <- 0
  url <- paste0("https://secure.shippingapis.com/ShippingAPI.dll?API=Verify&XML=<AddressValidateRequest USERID=\"", usps_userid, "\">
        <Revision>", rev,"</Revision>
        <Address>
            <Address1>", address_1,"</Address1>
            <Address2>", address_2,"</Address2>
            <City>", city,"</City>
            <State>", state,"</State>
            <Zip5>", zip5,"</Zip5>
            <Zip4>", zip4,"</Zip4>
        </Address>
        </AddressValidateRequest>") %>%
    stringr::str_replace_all("(\n|\\s+)", " ") %>%
    utils::URLencode()
  ls <- xml2::as_list(xml2::read_xml(url))
  t(as.data.frame(lapply(ls$AddressValidateResponse, unlist)))
}
