# Given a segment from a Datavyu data.frame, return the Databrary URL
# for that segment.
url_from_segment <- function(segment, vol_id, slot_id, asset_id) {

  paste0('https://nyu.databrary.org/slot/', slot_id,
                '/', onset_offset(segment, vol_id, slot_id, asset_id),'/asset/', 
                asset_id, '/download?inline=true')
}

onset_offset <- function(segment, vol_id, slot_id, asset_id) {
  seg_range <- databraryapi::get_asset_segment_range(vol_id = vol_id, 
                                                     session_id = slot_id, 
                                                     asset_id = asset_id)
  # TODO: Check login status
  if (is.null(seg_range)) {
    seg_range <- c(0,0)
  }
  
  onset_ms <- as.numeric(segment$onset, units='secs')*1000 + seg_range[1]
  offset_ms <- as.numeric(segment$offset, units='secs')*1000 + seg_range[1]
  on_off <- paste0(onset_ms, ',', offset_ms)
  if (is.null(on_off)) {
    on_off <- '-'
  }
  return(on_off)
}
