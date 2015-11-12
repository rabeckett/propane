module Policy


type Constraint = 
    | PathSelection of Prefix.T * Regex.T list 
    | RouteSummary of Prefix.T * Regex.T * Regex.T
    | CommunityTag of Prefix.T * Regex.T * Regex.T
    | MaxAdvertisements of int