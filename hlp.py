
'''IMPORTS'''

import webbrowser
import rasterio as rio
from rasterio.plot import show
import pandas as pd

def plot_site_scene(site_id,sites_df,raster_path):
    
    ''' Function to plot single site monitoring point, lake boundary and retrieved satellite image
        -site_id: site id from in-situ data
        -sites_df: geopandas dataframe containing the geometry for 
            the point ('geometry_ll') and the lake polygon ('geometry_poly')
        -raster_path: path of raster file to read'''
    
    scid = raster_path.split('__')[1]
    
    if 'sid' in sites_df.columns:
        
        site = sites_df[sites_df.sid==site_id] 
        
    else:
        
        sites_df = sites_df.reset_index()
        site = sites_df[sites_df.sid==site_id]

        
    with rio.open(raster_path) as src:

        point = site.to_crs(src.crs)
        poly = site.set_geometry('geometry_poly').to_crs(src.crs)

        fig, ax = plt.subplots(figsize=(9,9))

        show(src,ax=ax)
        poly.plot(ax=ax,color='b', alpha=0.01)
        point.plot(ax=ax,color='r')
        plt.title(f'{site_id}\n{scid}')





def browse_site(sites_df,site_id):

    '''sites_df: data frame containing site lon and lat'''
    '''site_id: site id (sid) as string'''
        
    
    
    if 'sid' in sites_df.columns:
        
        lon = "%.6f"%sites_df[sites_df.sid==site_id].lon.unique()
        lat = "%.6f"%sites_df[sites_df.sid==site_id].lat.unique()     
        
    else:
        
        sites_df = sites_df.reset_index()
        lon = "%.6f"%sites_df[sites_df.sid==site_id].lon.unique()
        lat = "%.6f"%sites_df[sites_df.sid==site_id].lat.unique()

    url = f'https://www.google.com/maps/search/{lat},{lon}'
    # Open URL in new browser window
    webbrowser.open_new(url)
    
    
def hide_spines(ax, positions=['top', 'right']):
    """
    Pass a matplotlib axis and list of positions with spines to be removed
    
    args:
        ax:          Matplotlib axis object
        positions:   Python list e.g. ['top', 'bottom']
    """
    assert isinstance(positions, list), 'Position must be passed as a list '
    
    for position in positions:
        ax.spines[position].set_visible(False)
    