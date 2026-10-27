### --- Package Import --- ####
from pathlib import Path
from icoscp_core.sites import bootstrap

cookie_token='fieldsitesToken=WzE3OTMyMTY4NzIxMTksIkNsZW1lbnMuS2xhbnRlQHN5ZHZhdHRlbi5zZSIsIlBhc3N3b3JkIl0eMGUCMFCjZxC/Zw5fN7xKobABeGTXOcotaEqmZnJvUEjVAktzGu7TpEhWcrxEWPtYtz2mLQIxAN046ztzhKWqkCBo+A+WUjzSOzYPWj7QUekAmR1J8teHErq39/qplQZ9/fBoiqbiVQ=='
meta, data = bootstrap.fromCookieToken(cookie_token)

# Check the authentication and make explicit


BOL_station = "https://meta.fieldsites.se/resources/stations/bolmen"
BOL_lake_site = "https://meta.fieldsites.se/resources/sites/bolmen-lake"

OUT_DIR = Path("../../data/")

def at_site(dobj_meta, site_uri: str) -> bool:
    site = dobj_meta.specificInfo.acquisition.site
    return site is not None and site.self.uri == site_uri

OUT_DIR.mkdir(exist_ok=True)

dobjs = meta.list_data_objects(station=BOL_station, limit=10000)

selected = [d for d in dobjs if at_site(meta.get_dobj_meta(d), BOL_lake_site)]

print(f"{len(selected)} of {len(dobjs)} Bolmen data objects are from Bolmen - Lake")

for d in selected:
    if (OUT_DIR / d.filename).exists():
        print(f"  skip (exists): {d.filename}")
        continue
    path = data.save_to_folder(d, str(OUT_DIR))
    print(f"  saved: {path}")