### --- Package Import --- ####
import os

from pathlib import Path
from icoscp_core.sites import bootstrap

from dotenv import load_dotenv

# load .env
load_dotenv(".env")

# Authentication
meta, data = bootstrap.fromCredentials(
    os.getenv('SITES_USER'), 
    os.getenv('SITES_PW')
)

# Define station and sites of interest 
BOL_Station = "https://meta.fieldsites.se/resources/stations/bolmen"
BOL_Site_Lake = "https://meta.fieldsites.se/resources/sites/bolmen-lake"
BOL_Site_Catch = "https://meta.fieldsites.se/resources/sites/bolmen-catchment-forest"

# Define dir where files should be stored and create if not exist
out_dir = Path(os.getenv('OUT_DIR'))
out_dir.mkdir(parents=True, exist_ok=True)


# Check if Site is not none and return
def at_site(dobj_meta, site_uri: str) -> bool:
    site = dobj_meta.specificInfo.acquisition.site
    return site is not None and site.self.uri == site_uri

# Get list of all data objects 
dobjs = meta.list_data_objects(station=BOL_Station)

# Select data objects where site location is matching
selected = [d for d in dobjs if at_site(meta.get_dobj_meta(d), BOL_Site_Lake)]

print(f"{len(selected)} of {len(dobjs)} data objects are from {BOL_Site_Lake}")

# Store files in out_dir if not existent
for d in selected:
    if (out_dir / d.filename).exists():
        print(f"  skip (exists): {d.filename}")
        continue
    path = data.save_to_folder(d, str(out_dir))
    print(f"  saved: {path} in {out_dir}" )