#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import geopandas as gpd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
import pyproj
import datetime
from statistics import mode
from geopy.geocoders import Nominatim


# In[ ]:





# # EDA - Exploratory Data Analysis on Milan listings

# In[ ]:





# ## LISTING VARIABLE

# In the following script the variable that are more recurrent in literature are analysed.  
# In particular, some exploratory data analysis are performed on the variable *price*, *room_type*, *neighbourhood* and *location*.

# In[2]:


#import listing data
list_jan = pd.read_csv('milano_2019/listings.csv') 
list_feb = pd.read_csv('milano_2019/listings-2.csv')
list_mar = pd.read_csv('milano_2019/listings-3.csv')
list_apr = pd.read_csv('milano_2019/listings-4.csv')
list_may = pd.read_csv('milano_2019/listings-5.csv')
list_jun = pd.read_csv('milano_2019/listings-6.csv')
list_jul = pd.read_csv('milano_2019/listings-7.csv')
list_aug = pd.read_csv('milano_2019/listings-8.csv')
list_sep = pd.read_csv('milano_2019/listings-9.csv')
list_oct = pd.read_csv('milano_2019/listings-10.csv')
list_nov = pd.read_csv('milano_2019/listings-11.csv')
list_dec = pd.read_csv('milano_2019/listings-12.csv')

Add a column to remind from which monthly update I got the data (some listing are present all day long, some just at the beginning, some just at the end):
> min(g2019$date)
 "2019-01-16"
> min(feb2019$date)
[1] "2019-02-07"
> min(mar2019$date)
[1] "2019-03-08"
> min(apr2019$date)
[1] "2019-04-11"
> min(may2019$date)
[1] "2019-05-14"
> min(jun2019$date)
[1] "2019-06-08"
> min(jul2019$date)
[1] "2019-07-12"
> min(aug2019$date)
[1] "2019-08-12"
> min(sep2019$date)
[1] "2019-09-20"
> min(oct2019$date)
[1] "2019-10-16"
> min(nov2019$date)
[1] "2019-11-16"
> min(dic2019$date)
[1] "2019-12-11"
# In[3]:


list_jan['last_update'] = '2019-01-16'
list_feb['last_update'] = '2019-02-07'
list_mar['last_update'] = '2019-03-08'
list_apr['last_update'] = '2019-04-11'
list_may['last_update'] = '2019-05-14'
list_jun['last_update'] = '2019-06-08'
list_jul['last_update'] = '2019-07-12'
list_aug['last_update'] = '2019-08-12'
list_sep['last_update'] = '2019-09-20'
list_oct['last_update'] = '2019-10-16'
list_nov['last_update'] = '2019-11-16'
list_dec['last_update'] = '2019-12-11'


# In[4]:


data_frames = [list_jan, list_feb, list_mar, list_apr, list_may, list_jun, list_jul,
              list_aug, list_sep, list_oct, list_nov, list_dec]


df_merged = pd.concat(data_frames)


# In[5]:


df_merged.head()


# In[6]:


len(df_merged)


# In[7]:


# some listing change 

room = df_merged.groupby('id').agg(modal_room_type = ('room_type', 
                                                   lambda x:x.value_counts().index[0])).reset_index()


# In[8]:


room.modal_room_type.unique()


# In[9]:


df_merged = df_merged.merge(room, how='left', on=['id'])


# In[10]:


list_room = df_merged.drop_duplicates(['id'])
list_room.head()


# In[11]:


# check for lst update:
list_room.last_update.unique()

# as mentioned, listing are added in the dataset each different month


# In[12]:


# the final dataset has a single row for listing
len(list_room), len(list_room.id.unique())


# In[13]:


list_room.modal_room_type.unique()


# In[14]:


list_room.groupby('modal_room_type').count()


# ### Type of listing

# In[15]:


# Plot the room types

list_room = df_merged.drop_duplicates(['id'])
sns.set(style="whitegrid")

fig, ax = plt.subplots()
fig.set_size_inches(12, 8)
fig.set_dpi(200)
p = sns.color_palette("colorblind", 3)

ax.axes.set_title("Room Type, Airbnb Milan",fontsize=20, pad=10)
ax = sns.countplot(x='modal_room_type',data=list_room,
                   order=list_room['modal_room_type'].value_counts().index, palette=p)

ax.set_xlabel('Number of Listings',fontsize=18,labelpad=10)
ax.set_ylabel('Type of Room',fontsize=18,labelpad=10)
ax.xaxis.set_tick_params(labelsize=15)
ax.yaxis.set_tick_params(labelsize=15)
# ax.annotate('Source: Airbnb - Get data', xy= (1, 0.05), xycoords='figure fraction', 
# horizontalalignment='center', verticalalignment='top', fontsize=8, color='#555555')


total = float(len(list_room))
for p in ax.patches:
    percentage = '{:.2f}%'.format(100 * p.get_height()/total)
    text = str(p.get_height()) + ' (' + percentage + ')'
    ax.annotate(text, (p.get_x()+0.4, p.get_height()+100), ha='center', size = 15)

    
#plt.savefig('room_type.png')


# In[ ]:





# In[ ]:





# ### Location of listing

# In[16]:


# Plot top 15 neighbourhoods in terms of listings

sns.set(style="whitegrid")
fig, ax = plt.subplots()
fig.set_size_inches(18, 12)
fig.set_dpi(200)
p = sns.color_palette("colorblind", 15)

ax.axes.set_title("Most Popular Neighourhoods in Milan",fontsize=20,pad=10)
ax = sns.countplot(x='neighbourhood',data=list_room, 
                   order = list_room['neighbourhood'].value_counts().iloc[:15].index, palette=p)

ax.set_xlabel('Number of Listings',fontsize=10,labelpad=10)
ax.set_ylabel('Neighbourhood',fontsize=10,labelpad=10)
ax.xaxis.set_tick_params(labelsize=10, rotation=90)
ax.yaxis.set_tick_params(labelsize=10)
# ax.annotate('Source: Airbnb - Get data', xy= (1, 0.05), xycoords='figure fraction', 
# horizontalalignment='center', verticalalignment='top', fontsize=8, color='#555555')


total = float(len(list_room))
for p in ax.patches:
    percentage = '{:.2f}%'.format(100 * p.get_height()/total)
    text = str(p.get_height()) + ' (' + percentage + ')'
    ax.annotate(text, (p.get_x()+0.4, p.get_height()+10), ha='center', size = 10)
    
#plt.savefig('Neighbourhood.png') saved manually in order not to cut the x axis


# In[17]:


list_room.head()


# In[18]:


district = list_room['neighbourhood'].value_counts()


# In[19]:


district


# In[ ]:





# In[20]:


district = district.to_frame().reset_index()


# In[21]:


district.columns


# In[22]:


district.rename(columns={'neighbourhood':'total', 'index':'neighbourhood'}, inplace=True)
district.head()


# In[ ]:





# In[23]:


# upload the geojson file for plotting the city of Milan
fname = "neighbourhoods.geojson"
city = gpd.read_file(fname)


# In[24]:


city_milan = city.merge(district, how='left', on='neighbourhood')


# In[25]:


city_milan


# In[26]:


# heat map of Milan according to number of listing per district

vmin, vmax = 0, 2500
fig, ax = plt.subplots(1, figsize=(22, 16))

city_milan.plot(column=city_milan['total'], cmap= 'YlOrRd', linewidth=0.8, ax=ax, edgecolor='0.8')

# add a title
ax.set_title('Listing per neighbourhood - Milan', fontdict={'fontsize': 30, 'fontweight' : 3})
# create an annotation for the data source
ax.annotate('Source: Airbnb - Get data', xy= (0.70, .05), xycoords='figure fraction', 
            horizontalalignment='center', verticalalignment='top', fontsize=20, color='#555555')
            
        
# Create colorbar as a legend
sm = plt.cm.ScalarMappable(cmap='YlOrRd', norm=plt.Normalize(vmin=vmin, vmax=vmax))
# empty array for the data range
#sm._A = []
# add the colorbar to the figure
cbar = fig.colorbar(sm)

city_milan['coords'] = city_milan['geometry'].apply(lambda x: x.representative_point().coords[:])
city_milan['coords'] = [coords[0] for coords in city_milan['coords']]

for idx, row in city_milan.iterrows():
    plt.annotate(s=row['neighbourhood'], xy=row['coords'], horizontalalignment='center', size=10)
    
#fig.savefig("listing_milan2.png", dpi=300)


# In[27]:


list_room.head()


# In[28]:


list_room.number_of_reviews.max()


# In[29]:


gdf = list_room.iloc[:, [0,1,6,7,17]]
gdf = gpd.GeoDataFrame(
    gdf, geometry=gpd.points_from_xy(gdf.longitude, gdf.latitude))


# In[30]:


gdf.head()


# In[31]:


gdf.modal_room_type.unique()


# In[32]:


# scatter

# sources: https://geopandas.readthedocs.io/en/latest/gallery/create_geopandas_from_pandas.html
#          https://www.earthdatascience.org/courses/scientists-guide-to-plotting-data-in-python/
# plot-spatial-data/customize-vector-plots/python-customize-map-legends-geopandas/


fig, ax = plt.subplots(1, figsize=(22, 16))

city_milan.plot(ax = ax, color='whitesmoke', edgecolor='grey')

room_palette = {'Private room':'limegreen', 'Entire home/apt':'gold', 
                'Shared room':'red', 'Hotel room': 'blueviolet'}            

for room, data in gdf.groupby('modal_room_type'):
    color = room_palette[room]
    label = room
    data.plot(color=color,
              ax=ax,
              label=label,
              markersize=20,
              #alpha=0.5,
              edgecolors='k')
    
ax.legend(fontsize=20,
          frameon=True,
          loc=(0.001,0.001),
          title="LEGEND")



ax.set_title('Airbnb listings in Milan', fontdict={'fontsize': 30, 'fontweight' : 3})

ax.annotate('Source: Airbnb - Get data', xy= (0.8, .15), xycoords='figure fraction', 
            horizontalalignment='center', verticalalignment='top', fontsize=20, color='#555555')


#fig.savefig("scatter.png", dpi=300)

plt.show()


# In[ ]:





# In[33]:


fig, axes = plt.subplots(2, 2, figsize=(35, 25))

axes[0, 0].set_title('Entire home/apt')
city_milan.plot(ax = axes[0, 0], color='whitesmoke', edgecolor='grey')
gdf[gdf['modal_room_type'] == 'Entire home/apt'].plot(ax = axes[0, 0], 
                                                color='gold', 
                                                markersize=15, 
                                                alpha=0.5,
                                                edgecolors='k')

axes[0,1].set_title('Private Room')
city_milan.plot(ax = axes[0, 1], color='whitesmoke', edgecolor='grey')
gdf[gdf['modal_room_type'] == 'Private room'].plot(ax = axes[0, 1], 
                                             color='limegreen', 
                                             markersize=15,
                                             alpha=0.5,
                                             edgecolors='k')

axes[1, 0].set_title('Hotel Room')
city_milan.plot(ax = axes[1,0], color='whitesmoke', edgecolor='grey')
gdf[gdf['modal_room_type'] == 'Hotel room'].plot(ax = axes[1,0], 
                                            color='blueviolet', 
                                            markersize=15,
                                            alpha=0.5,

                                                 edgecolors='k')

axes[1, 1].set_title('Shared Room')
city_milan.plot(ax = axes[1, 1], color='whitesmoke', edgecolor='grey')
gdf[gdf['modal_room_type'] == 'Shared room'].plot(ax = axes[1,1], 
                                            color='red', 
                                            markersize=15,
                                            alpha=0.5,
                                            edgecolors='k')

axes[1,1].annotate('Source: Airbnb - Get data', xy= (0.80, .1), xycoords='figure fraction', horizontalalignment='center', verticalalignment='top', fontsize=20, color='#555555')

#fig.savefig("scatter_piccoli.png", dpi=300)


plt.show()


# In[ ]:





# In[34]:


# price distribution

# barplot for price in the city


# In[ ]:





# In[35]:


df_merged.head()

During the year, listing might change the price. In order to have a unique value on which compute average price per district and room type, we use the mode.
# In[36]:


price = df_merged.groupby('id').agg(modal_price = ('price', 
                                                   lambda x:x.value_counts().index[0])).reset_index()
price_df = df_merged.merge(price, how='left', on=['id'])


# In[37]:


price.head()


# In[38]:


avg_price = price.modal_price.mean()
round(avg_price)


# In[39]:


len(price) == len(df_merged.id.unique())


# In[40]:


# average price in each neighbourhood

quartieri = price_df.groupby('neighbourhood').agg(avg = ('modal_price', 'mean')).reset_index()
quartieri_sort = quartieri.sort_values(by = 'avg', ascending = False).head(15)


# In[41]:


# plot the 15 most expensive district in Milan with their price

sns.set(style="whitegrid")
fig, ax = plt.subplots()
fig.set_size_inches(18, 12)
fig.set_dpi(200)
p = sns.color_palette("colorblind", 15)

ax.axes.set_title("Most expensive neighbourhood in Milan",fontsize=20,pad=10)
ax = sns.barplot(x="neighbourhood", y="avg", data=quartieri_sort, palette = p)

ax.set_xlabel('Price (euro)',fontsize=10,labelpad=10)
ax.set_ylabel('Neighbourhood',fontsize=10,labelpad=10)
ax.xaxis.set_tick_params(labelsize=10, rotation=90)
ax.yaxis.set_tick_params(labelsize=10)
# ax.annotate('Source: Airbnb - Get data', xy= (1, 0.05), xycoords='figure fraction', 
# horizontalalignment='center', verticalalignment='top', fontsize=8, color='#555555')

# Go to the next section for media_noout
media_noout = 102.858
plt.annotate(s="Average price in the city (removing the effect of outliers)", 
             xy=(-0.2, 95), 
             bbox=dict(boxstyle="round",alpha=0.75,facecolor = "white",edgecolor = "grey"),)
ax.axhline(y=round(media_noout), linestyle = '--', color = 'black')


plt.annotate(s="Average price in the city", 
             xy=(-0.2, 120), 
             bbox=dict(boxstyle="round",alpha=0.75,facecolor = "white",edgecolor = "grey"),)
ax.axhline(y=round(avg_price), linestyle = '--', color = 'black')


for p in ax.patches:
    text = str(round(p.get_height())) + '€'
    ax.annotate(text, (p.get_x()+0.4, p.get_height()+1), ha='center', size = 10)
    
#plt.savefig('price.png')


# In[ ]:





# In[42]:


city_milan = city_milan.merge(quartieri, how='left', on='neighbourhood')
city_milan.sort_values(by = 'avg', ascending = False).head(15)


# In[43]:


city_milan.avg.min(), city_milan.avg.max()


# In[44]:


# heat map of Milan according to average price of listing per district

vmin, vmax = 30, 300
fig, ax = plt.subplots(1, figsize=(22, 16))

city_milan.plot(column=city_milan['avg'], cmap= 'Spectral_r', linewidth=0.8, ax=ax, edgecolor='0.8')

# add a title
ax.set_title('Average price per neighbourhood - Milan', fontdict={'fontsize': 30, 'fontweight' : 3})
# create an annotation for the data source
ax.annotate('Source: Airbnb - Get data', xy= (0.70, .05), xycoords='figure fraction', 
            horizontalalignment='center', verticalalignment='top', fontsize=20, color='#555555')
            
        
# Create colorbar as a legend
sm = plt.cm.ScalarMappable(cmap='Spectral_r', norm=plt.Normalize(vmin=vmin, vmax=vmax))
# empty array for the data range
#sm._A = []
# add the colorbar to the figure
cbar = fig.colorbar(sm)

city_milan['coords'] = city_milan['geometry'].apply(lambda x: x.representative_point().coords[:])
city_milan['coords'] = [coords[0] for coords in city_milan['coords']]

for idx, row in city_milan.iterrows():
    plt.annotate(s=row['neighbourhood'], xy=row['coords'], horizontalalignment='center', size=10)
    
#fig.savefig("avgprice_neighbourhood.png", dpi=300)


# In[45]:


# same heat map removing outliers
media, stdeviation = price.modal_price.mean(), price.modal_price.std()

# outlier are here identified as observation that lies three standard deviation
# from the average

# !!! I could have used the interquartile range as well

interval = stdeviation * 3
lower, upper = media - interval, media + interval


# In[46]:


price_no_out = price.loc[(price['modal_price'] > lower) & (price['modal_price'] < upper)]


# In[47]:


price_no_out.head()


# In[48]:


price_no_out.modal_price.min(), price_no_out.modal_price.max()


# In[49]:


price_no_out = df_merged.groupby('id').agg(modal_price = ('price', 
                                                   lambda x:x.value_counts().index[0])).reset_index()
price_df_no_out = df_merged.merge(price_no_out, how='left', on=['id'])


# In[50]:


price_df_no_out.head()


# In[51]:


city_milan


# In[ ]:





# In[52]:


# average price in each neighbourhood (no outliers)

quartieri_noout = price_df_no_out.groupby('neighbourhood').agg(avg_noout = ('modal_price', 'mean')).reset_index()
quartieri_sort_noout = quartieri_noout.sort_values(by = 'avg_noout', ascending = False).head(15)


# In[53]:


quartieri_noout.head()


# In[54]:


city_milan_noout = city_milan.merge(quartieri_noout, how='left', on='neighbourhood')


# In[55]:


city_milan_noout.head()


# In[ ]:





# In[56]:


#city_milan_noout.sort_values(by = 'avg_noout', ascending = False).head(15)


# In[57]:


city_milan_noout.avg_noout.min(), city_milan_noout.avg_noout.max()


# In[58]:


# heat map of Milan according to average price of listing per district

vmin, vmax = 30, 300
fig, ax = plt.subplots(1, figsize=(22, 16))

city_milan_noout.plot(column=city_milan_noout['avg_noout'], cmap= 'Spectral_r', 
                      linewidth=0.8, ax=ax, edgecolor='0.8')

# add a title
ax.set_title('Average price per neighbourhood - Milan NO OUTLIERS', fontdict={'fontsize': 30, 'fontweight' : 3})
# create an annotation for the data source
ax.annotate('Source: Airbnb - Get data', xy= (0.70, .05), xycoords='figure fraction', 
            horizontalalignment='center', verticalalignment='top', fontsize=20, color='#555555')
            
        
# Create colorbar as a legend
sm = plt.cm.ScalarMappable(cmap='Spectral_r', norm=plt.Normalize(vmin=vmin, vmax=vmax))
# empty array for the data range
#sm._A = []
# add the colorbar to the figure
cbar = fig.colorbar(sm)

city_milan_noout['coords'] = city_milan_noout['geometry'].apply(lambda x: x.representative_point().coords[:])
city_milan_noout['coords'] = [coords[0] for coords in city_milan_noout['coords']]

for idx, row in city_milan_noout.iterrows():
    plt.annotate(s=row['neighbourhood'], xy=row['coords'], horizontalalignment='center', size=10)
    
#fig.savefig("avgprice_neighbourhood_noOUT.png", dpi=300)

No great influence on the average per neighbourhood.
# In[ ]:





# In[59]:


# average price for type of room

room_type = price_df.groupby('room_type').agg(avg = ('modal_price', 'mean')).reset_index()
room_type.head()


# In[ ]:





# In[60]:


list_room = list_room.merge(price, how='left', on='id')
list_room.head()


# In[61]:


# price distribution

fig, ax = plt.subplots(figsize=(12,8))
ax.hist(list_room.modal_price,bins=50)
ax.set_xlabel('Price')
ax.set_ylabel('Number of Properties')
ax.set_title('Distribution of Airbnb Rental Property Prices in Milan')

#fig.savefig("price_dist.png", dpi=300)


# In[62]:


# Clearly, the outliers drive my plot to the right and do not Delete outlier in order to plot distribution.
# I therefore decide to delete them to create a more explicative graph


# In[63]:


list_room.modal_price.min(), list_room.modal_price.max()


# In[64]:


media, stdeviation = list_room.modal_price.mean(), list_room.modal_price.std()


# In[65]:


# outliers are removed

interval = stdeviation * 3
lower, upper = media - interval, media + interval


# In[66]:


list_room_price = list_room.loc[(list_room['modal_price'] > lower) & (list_room['modal_price'] < upper)]


# In[67]:


list_room_price.modal_price.min(),list_room_price.modal_price.max()


# In[68]:


len(list_room), len(list_room_price)

By deleting just a small number of elements, I am able to correct for the effect of outliers.
# In[69]:


media_noout, stdeviation_noout = list_room_price.modal_price.mean(), list_room_price.modal_price.std()
media_noout, stdeviation_noout 


# In[ ]:





# In[ ]:





# In[70]:


fig, ax = plt.subplots(figsize=(12,8), dpi=100)
ax.hist(list_room_price.modal_price,bins=50)
ax.set_xlabel('Price')
ax.set_ylabel('Number of Properties')
ax.set_title('Distribution of Airbnb Rental Property Prices in Milan (without outliers)', 
             fontdict={'fontsize': 15, 'fontweight' : 3})
ax.annotate('Source: Airbnb - Get data', xy= (0.8, 0.05), xycoords='figure fraction', 
            horizontalalignment='center', verticalalignment='top', fontsize=10, color='#555555')


#fig.savefig("price_dist-noout.png", dpi=300)


# In[ ]:





# In[71]:


fig, ax = plt.subplots(figsize=(12,8), dpi=100)
ax.hist(list_room_price.modal_price,bins=100)
ax.set_xlabel('Price')
ax.set_ylabel('Number of Properties')
ax.set_title('Distribution of Airbnb Rental Property Prices in Milan (without outliers)', 
             fontdict={'fontsize': 15, 'fontweight' : 3})
ax.annotate('Source: Airbnb - Get data', xy= (0.8, 0.05), xycoords='figure fraction', 
            horizontalalignment='center', verticalalignment='top', fontsize=10, color='#555555')


#fig.savefig("price_dist-noout_due.png", dpi=300)


# In[ ]:





# In[ ]:





# In[72]:


# check for price distribution per neighbourhood NO OUTLIERS

order = quartieri.sort_values(by = 'avg', ascending = False).reset_index().neighbourhood


# In[73]:


order


# In[ ]:





# In[74]:


plt.style.use('seaborn-white')
plt.figure(figsize=(14,12), dpi= 100)
sns.boxplot(y='modal_price',x='neighbourhood',data = list_room_price, order = order)
plt.title('Neighbourhood Group Price Distribution', fontdict={'fontsize': 18,
 'fontweight':3})
plt.ylabel('Price')
plt.xlabel('Neighbourhood')
plt.xticks(rotation=90)
plt.show()

#plt.savefig('boxplot_neigh_price.png')


# In[ ]:





# In[75]:


plt.style.use('seaborn-white')
plt.figure(figsize=(10,8), dpi = 200)
sns.boxplot(y='modal_price',x='modal_room_type',data = list_room_price)
plt.title('Price Distribution for different type of room', fontdict={'fontsize': 18,
 'fontweight':3})
plt.ylabel('Price')
plt.xlabel('Type of room')
plt.xticks(rotation=60)
plt.show()

#plt.savefig('boxplot_room_price.png')


# In[ ]:





# In[76]:


# Reviews


# In[77]:


list_room_price.number_of_reviews.describe()


# In[78]:


# histogram with the total number of reviews

fig, ax = plt.subplots(figsize=(12,8), dpi=100)
ax.hist(list_room.number_of_reviews,bins=100)
ax.set_xlabel('reviews')
ax.set_ylabel('Number of listing')
ax.set_title('Number of Reviews - Distribution of Airbnb Rental Property in Milan', 
             fontdict={'fontsize': 15, 'fontweight' : 3})
ax.annotate('Source: Airbnb - Get data', xy= (0.8, 0.05), xycoords='figure fraction', 
            horizontalalignment='center', verticalalignment='top', fontsize=10, color='#555555')


#fig.savefig("reviews-totnum.png", dpi=300)


# In[79]:


# histogram with the number of reviews per month

fig, ax = plt.subplots(figsize=(12,8), dpi=100)
ax.hist(list_room.reviews_per_month,bins=100)
ax.set_xlabel('reviews')
ax.set_ylabel('Number of listing')
ax.set_title('Number of Reviews per month - Distribution of Airbnb Rental Property in Milan', 
             fontdict={'fontsize': 15, 'fontweight' : 3})
ax.annotate('Source: Airbnb - Get data', xy= (0.8, 0.05), xycoords='figure fraction', 
            horizontalalignment='center', verticalalignment='top', fontsize=10, color='#555555')


#fig.savefig("reviews-permonth.png", dpi=300)


# In[ ]:





# In[80]:


# reviews per neighbourhood

plt.style.use('seaborn-white')
plt.figure(figsize=(14,12), dpi= 100)
sns.boxplot(y='number_of_reviews',x='neighbourhood',data = list_room, order = order)
plt.title('Neighbourhood Group Distribution of number of reviews', fontdict={'fontsize': 18,
 'fontweight':3})
plt.xticks(rotation=90)
plt.ylabel('Number of reviews')
plt.xlabel('Neighbourhood')
plt.show()


# In[81]:


plt.style.use('seaborn-white')
plt.figure(figsize=(10,8), dpi = 200)
sns.boxplot(y='number_of_reviews',x='modal_room_type',data = list_room)
plt.title('Distribution of reviews for different type of room', fontdict={'fontsize': 18,
 'fontweight':3})
plt.ylabel('Number of reviews')
plt.xlabel('Type of room')
plt.xticks(rotation=60)
plt.show()


# In[82]:


# different scatter to check for any relationship between price and reviews


# In[83]:


plt.style.use('seaborn-white')
plt.figure(figsize=(10,8), dpi = 200)

colors = {'Private room':'limegreen', 'Entire home/apt':'gold', 
                'Shared room':'red', 'Hotel room': 'blueviolet'}            

for tipo in list_room_price.modal_room_type.unique():
    x = list_room_price[list_room_price['room_type'] == tipo].modal_price
    y = list_room_price[list_room_price['room_type'] == tipo].number_of_reviews
    plt.scatter(x, y, c=colors[tipo], edgecolors='b', alpha=0.5)

plt.title('Scatterplot of price over reviews', fontdict={'fontsize': 18,
 'fontweight':3})

plt.legend(list_room_price.modal_room_type.unique(), bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.grid(True)
plt.ylabel('Total number of reviews')
plt.xlabel('Price')
plt.xticks(rotation=60)
plt.show()

#plt.savefig("scatter_review-prezzo.png", dpi=300)


# In[84]:


fig, axes = plt.subplots(2, 2, figsize=(35, 25))

axes[0, 0].set_title('Entire home/apt')
x = list_room_price[list_room_price['room_type'] == 'Entire home/apt'].modal_price
y = list_room_price[list_room_price['room_type'] == 'Entire home/apt'].number_of_reviews
axes[0, 0].scatter(x, y, c=colors['Entire home/apt'], edgecolors='b', alpha=0.5)


axes[0,1].set_title('Private Room')
x = list_room_price[list_room_price['room_type'] == 'Private room'].modal_price
y = list_room_price[list_room_price['room_type'] == 'Private room'].number_of_reviews
axes[0, 1].scatter(x, y, c=colors['Private room'], edgecolors='b', alpha=0.5)


axes[1, 0].set_title('Hotel Room')
x = list_room_price[list_room_price['room_type'] == 'Hotel room'].modal_price
y = list_room_price[list_room_price['room_type'] == 'Hotel room'].number_of_reviews
axes[1, 0].scatter(x, y, c=colors['Hotel room'], edgecolors='b', alpha=0.5)


axes[1, 1].set_title('Shared Room')
x = list_room_price[list_room_price['room_type'] == 'Shared room'].modal_price
y = list_room_price[list_room_price['room_type'] == 'Shared room'].number_of_reviews
axes[1, 1].scatter(x, y, c=colors['Shared room'], edgecolors='b', alpha=0.5)


axes[1,1].annotate('Source: Airbnb - Get data', xy= (0.80, .1), xycoords='figure fraction', horizontalalignment='center', verticalalignment='top', fontsize=20, color='#555555')

#fig.savefig("scatter_piccoli_review-prezzo.png", dpi=300)


# In[ ]:





# In[85]:


plt.style.use('seaborn-white')
plt.figure(figsize=(10,8), dpi = 200)

colors = {'Private room':'limegreen', 'Entire home/apt':'gold', 
                'Shared room':'red', 'Hotel room': 'blueviolet'}            

for tipo in list_room_price.modal_room_type.unique():
    x = list_room_price[list_room_price['room_type'] == tipo].modal_price.apply(np.log)
    y = list_room_price[list_room_price['room_type'] == tipo].number_of_reviews
    plt.scatter(x, y, c=colors[tipo], edgecolors='b', alpha=0.5)

plt.suptitle("Scatterplot of price over reviews", fontsize=16, horizontalalignment = 'center')

plt.title("Price logaritmic transformation", fontsize=10, loc = 'left')

plt.legend(list_room_price.modal_room_type.unique(), bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.grid(True)
plt.ylabel('Total number of reviews')
plt.xlabel('Price')
plt.xticks(rotation=60)
plt.show()
#fig.savefig("price_log-scatter.png", dpi=300)


# In[ ]:





# In[86]:


# checking if the average has some differences


# In[87]:


plt.style.use('seaborn-white')
plt.figure(figsize=(10,8), dpi = 200)

colors = {'Private room':'limegreen', 'Entire home/apt':'gold', 
                'Shared room':'red', 'Hotel room': 'blueviolet'}            

for tipo in list_room_price.modal_room_type.unique():
    x = list_room_price[list_room_price['room_type'] == tipo].modal_price
    y = list_room_price[list_room_price['room_type'] == tipo].reviews_per_month
    plt.scatter(x, y, c=colors[tipo], edgecolors='b', alpha=0.5)

plt.title('Scatterplot of price over reviews', fontdict={'fontsize': 18,
 'fontweight':3})

plt.legend(list_room_price.modal_room_type.unique(), bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.grid(True)
plt.ylabel('Total number of reviews')
plt.xlabel('Price')
plt.xticks(rotation=60)
plt.show()

#plt.savefig("scatter_review-prezzo.png", dpi=300)


# In[88]:


plt.style.use('seaborn-white')
plt.figure(figsize=(10,8), dpi = 200)

colors = {'Private room':'limegreen', 'Entire home/apt':'gold', 
                'Shared room':'red', 'Hotel room': 'blueviolet'}            

for tipo in list_room_price.modal_room_type.unique():
    x = list_room_price[list_room_price['room_type'] == tipo].modal_price.apply(np.log)
    y = list_room_price[list_room_price['room_type'] == tipo].reviews_per_month
    plt.scatter(x, y, c=colors[tipo], edgecolors='b', alpha=0.5)

plt.suptitle("Scatterplot of price over reviews", fontsize=16, horizontalalignment = 'center')

plt.title("Price logaritmic transformation", fontsize=10, loc = 'left')

plt.legend(list_room_price.modal_room_type.unique(), bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.grid(True)
plt.ylabel('Total number of reviews')
plt.xlabel('Price')
plt.xticks(rotation=60)
plt.show()

#fig.savefig("price_log-scatter.png", dpi=300)


# In[ ]:





# In[ ]:





# In[ ]:





# ## Data Cleaning and Feature Engineering

# Some interesting feature can be added to the data frame, in order to get the best fitting model while doing predictions.  
# From literature, somee interesting variables are:
# * distance from the city center;  
# * number of nearby amenities;  
# * ..
In order to compute the distance to the city center, we need to identify the point that will be taken as center.
For simplicity, we will use Duomo - see wikipedia.  

To reinforce this choiche, each listing offer shows the main attraction in the city and the distance
# #### Distance from the city center CBD

# In[89]:


geolocator = Nominatim(user_agent="myapplication")
address = 'Piazza del Duomo, 20122 Milano MI'
location = geolocator.geocode(address, timeout=30)
      
coords_duomo = (location.latitude, location.longitude)

geod = pyproj.Geod(ellps='WGS84')


# In[90]:


list_room['CBD'] = ''

for i in range(len(list_room)):
    lat0, lon0 = location.latitude, location.longitude
    lat1, lon1 = list_room['latitude'][i], list_room['longitude'][i]
    
    azimuth1, azimuth2, distance = geod.inv(lon0, lat0, lon1, lat1)
    
    list_room['CBD'][i] = round((distance/1000), 1)


# In[91]:


list_room.head()


# In[92]:


#list_room.to_csv('listing_2019_tot.csv', index=False)


# In[93]:


len(list_room), len(df_merged.id.unique())


# In[ ]:





# ## POINTS OF INTERESTS

# In[94]:


import folium
from shapely.geometry import Point
from shapely.geometry.polygon import Polygon


# In[97]:


from openrouteservice import client

api_key = '5b3ce3597851110001cf624892490e9aaed54f719d8183bf82fee6ef'
clnt = client.Client(key=api_key) 
map1 = folium.Map(tiles='cartodbpositron', location=([location.latitude, location.longitude]), zoom_start=10)


# In[98]:


# I create a dataset with pois
# data from https://theculturetrip.com/europe/italy/articles/20-must-visit-attractions-in-milan/

d1 = {"ID":1001373083,"type":["Museum"],"name":"Leonardo3, Milan",
      "post_status":"publish","domain":"attractions","lat":45.4665115,"lng":9.1897812}

d2 = {"ID":2015256,"type":["Historical Landmark"],"name":"Torre Branca, Milan",
      "post_status":"publish","domain":"attractions","lat":45.4732972,"lng":9.1729333}

d3 = {"ID":1001373100,"type":["Art Gallery"],"name":"Pinacoteca Ambrosiana",
 "post_status":"publish","domain":"attractions","lat":45.4634791,"lng":9.1857881}

d4 = {"ID":1979870,"type":["Building","Cathedral","Church"],"name":"Il Duomo",
      "post_status":"publish","domain":"attractions","lat":45.4640976,"lng":9.1919265}

d5 = {"ID":1979867,"type":["Historical Landmark"],"name":"Castello Sforzesco",
      "post_status":"publish","domain":"attractions","lat":45.4704762,"lng":9.1793325}

d6 = {"ID":1979868,"type":["Shopping Mall"],"name":"Galleria Vittorio Emanuele II",
       "post_status":"publish","domain":"attractions","lat":45.4658434,"lng":9.1899401}

d7 = {"ID":2053723,"type":["Opera House"],"name":"Teatro alla Scala",
      "post_status":"publish","domain":"attractions","lat":45.4674021,"lng":9.1895512}

d8 = {"ID":2014335,"type":["Church"],"name":"Santa Maria delle Grazie, Milan",
 "post_status":"publish","domain":"attractions","lat":45.465963,"lng":9.1709621}

d9 = {"ID":1001373088,"type":["Historical Landmark"],"name":"Navigli Neighbourhood",
      "post_status":"publish","domain":"attractions","lat":45.4477479,"lng":9.1696288}

d10 = {"ID":1979864,"type":["Art Gallery","Museum"],"name":"Museo del Novecento",
       "post_status":"publish","domain":"attractions","lat":45.4634871,"lng":9.1902915}

d11 = {"ID":1979871,"type":["Art Gallery","Architectural Landmark"],"name":"Pinacoteca di Brera, Milano",
       "post_status":"publish","domain":"attractions","lat":45.4719545,"lng":9.1878145}

d12 = {"ID":1979865,"type":["Park"],"name":"Parco Sempione, Milano",
       "post_status":"publish","domain":"attractions","lat":45.4720981,"lng":9.1772243}

d13 = {"ID":1979874,"type":["Church"],"name":"San Maurizio al Monastero Maggiore, Milan",
       "post_status":"publish","domain":"attractions","lat":45.4655747,"lng":9.1789404}

d14 = {"ID":1001373149,"type":["Cemetery"],"name":"Monumental Cemetery, Milan",
       "post_status":"publish","domain":"attractions","lat":45.4873357,"lng":9.1777588}

d15 = {"ID":1979863,"type":["Art Gallery","Cinema"],"name":"Fondazione Prada",
       "post_status":"publish","domain":"attractions","lat":45.4440639,"lng":9.2048171}

d16 = {"ID":1979873,"type":["Building"],"name":"Torre Velasca",
       "post_status":"publish","domain":"attractions","lat":45.4598961,"lng":9.1906166}

d17 = {"ID":1001373186,"type":["Historical Landmark"],"name":"Corso Como, Milan",
       "post_status":"publish","domain":"attractions","lat":45.4822154,"lng":9.185178317}


# In[99]:


ds = [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17]

d = {}
for k in d1.keys():
    d[k] = tuple(d[k] for d in ds)


# In[100]:


pois = pd.DataFrame.from_dict(d)


# In[101]:


pois.head(17)


# In[ ]:





# In[102]:


pois.icon = ['university', 'building', 'paint-brush', 'church', 'building', 'shopping-bag', 'university',
             'church', 'building', 'university', 'university', 'tree', 'church', 'cross', 
             'paint-brush', 'building', 'road']


# In[103]:


len(pois.icon)


# In[104]:


pois['icon'] = pois.icon


# In[ ]:





# In[105]:


for i in range(len(pois)):
    folium.map.Marker([pois['lat'][i], pois['lng'][i]],
                     icon=folium.Icon(color='lightgray', icon=pois['icon'][i], prefix='fa'),
                      popup=pois['name'][i]).add_to(map1)


# In[106]:


map1


# In[107]:


pois['point'] = list(zip(pois.lng, pois.lat))


# In[108]:


pois['point'] = pois['point'].apply(lambda p: Point(p))


# #### number of pois within 1 km AND average distance from all the 17 points of interest

# In[109]:


list_room['number_pois'] = 0
list_room['avg_distance'] = 0


# In[110]:


list_room.head(3)


# In[111]:


for j in range(len(pois)):
    for i in range(len(list_room)):
        lat0, lon0 = pois['lat'][j], pois['lng'][j]
        lat1, lon1 = list_room['latitude'][i], list_room['longitude'][i]
    
        azimuth1, azimuth2, distance = geod.inv(lon0, lat0, lon1, lat1)
        
        list_room['avg_distance'][i] += distance
    
        if distance <= 1000:
            list_room['number_pois'][i] += 1


# In[112]:


list_room.head(3)


# In[113]:


list_room['avg_distance'] = list_room['avg_distance'].apply(lambda x: round(((x/len(pois))/1000), 1))


# In[114]:


list_room.head(3)


# In[115]:


list_room.to_csv('list_room.csv')


# In[ ]:





# In[ ]:





# In[155]:


list_room[list_room.number_pois.isin([0])]


# In[ ]:





# In[ ]:


###################### Isochrones ######################


# As explained in Chapter 2.3.1, these methods do not work on such a big dataset.

# In[ ]:


#### isochrones in osmnx


# In[1]:


from shapely.geometry import Point, Polygon
from shapely.geometry.polygon import Polygon
import networkx as nx
import osmnx as ox
from pyproj import Proj, transform, Geod
ox.config(log_console=True, use_cache=True)


# In[ ]:


def get_isochrone(loc, walk_time=10, speed=4.5):
    #loc = (lat, lon)
    G = ox.graph_from_point(loc, simplify=True, network_type='walk')
    gdf_nodes = ox.graph_to_gdfs(G, edges=False)
    x, y = gdf_nodes['geometry'].unary_union.centroid.xy
    center_node = ox.get_nearest_node(G, (y[0], x[0]))
    meters_per_minute = speed * 1000 / 60 #km per hour to m per minute
    for u, v, k, data in G.edges(data=True, keys=True):
        data['time'] = data['length'] / meters_per_minute
    subgraph = nx.ego_graph(G, center_node, radius=walk_time, distance='time')
    node_points = [Point(data['x'], data['y']) for node, data in subgraph.nodes(data=True)]
    polys = gpd.GeoSeries(node_points).unary_union.convex_hull
    return polys


# In[ ]:


list_room['coord'] = list(zip(list_room.latitude, list_room.longitude))


# In[ ]:


list_room['iso_5min'] = list_room['coord'].apply(lambda x: get_isochrone(x, walk_time=12, speed=4.5))


# In[ ]:


list_room['count_iso'] = 0


# In[ ]:


# count the number of pois for isochrone

for j in range(len(pois)):
    for i in range(len(list_room)):
        p = list_room['iso_5min'][i]
        if p.contains(pois['point'][j]):
            #print(str(pois['name'][j]) + ' is in ' + str(list_room_sub['id'][i]))
            list_room['count_iso'][i] += 1
            j += 1


# In[ ]:


# print on the map (JUST FEW as example)

for i in range(len(list_room)):
    folium.GeoJson(list_room['iso_5min'][i]).add_to(map1)
    folium.map.Marker((list_room['coord'][i]), 
    icon=folium.Icon(color='gray', 
                     icon_color='#cc0000', 
                     icon='home', prefix='fa')).add_to(map1)


# In[ ]:


#### isochrones in OPENROUTESERVICE


# In[309]:



pois['point'] = pois['point'].apply(lambda p: Point(p))


# In[310]:


# point of interest of the city
pois.head()


# In[381]:


coords_duomo


# In[ ]:





# In[311]:


# set up a disctionary with coords


# In[312]:


from geojson import Feature, Point, FeatureCollection, dump

listing_dict = {}

for index, row in list_room.iterrows():
    listing_dict[row.id] = {'location': [row.longitude, row.latitude]}


# In[318]:


#listing_dict


# In[313]:


########################## trials with first 5 listings ##########################

list_room_subset = list_room.iloc[1:6, ]


# In[314]:


subset_dict = {}

for index, row in list_room_subset.iterrows():
    subset_dict[row.id] = {'location': [row.longitude, row.latitude]}


# In[259]:


subset_dict


# In[ ]:


##################################################################################


# In[ ]:





# In[383]:


params_iso = {'profile': 'foot-walking',
              'intervals': [600], # 600/60 = 10 mins
              'segments': 600
             }

for listing, coord in subset_dict.items():
    params_iso['locations'] = [coord['location']] # Add apartment coords to request parameters
    coord['iso'] = clnt.isochrones(**params_iso) # Perform isochrone request
    folium.features.GeoJson(coord['iso']).add_to(map1) # Add GeoJson to map
    
    folium.map.Marker(list(reversed(coord['location'])), # reverse coords due to weird folium lat/lon syntax
                      icon=folium.Icon(color='lightgray',
                                        icon_color='#cc0000',
                                        icon='home',
                                        prefix='fa',
                                       ),
                      popup=listing,
                 ).add_to(map1) # Add apartment locations to map

map1


# In[ ]:





# In[301]:


from geojson import Point, Feature, FeatureCollection, dump

#s = subset_dict[23986]['iso']['features'][0]['geometry']
#key = 23986

features = []

for key in subset_dict:
    g = subset_dict[23986]['iso']['features'][0]['geometry']
    
    features.append(Feature(geometry=g, properties={"id": key}))


feature_collection = FeatureCollection(features)

#with open('myfile.geojson', 'w') as f:
#    dump(feature_collection, f)


# In[321]:


polygon = gpd.GeoDataFrame.from_features(feature_collection['features'])


# In[338]:


polygon.head()


# In[320]:


pois.head()


# In[337]:


from shapely.geometry import Point
from shapely.geometry.polygon import Polygon

polygon['count'] = 0

for j in range(len(pois)):
    for i in range(len(polygon)):
        p = polygon['geometry'][i]
        if p.contains(pois['point'][j]):
            print(str(pois['name'][j]) + ' is in ' + str(polygon['id'][i]))
            polygon['count'][i] += 1
            j += 1
    


# In[ ]:


##############################################################################


# In[ ]:





# ## CALENDAR

# In[163]:


#import calendar data
cal_jan = pd.read_csv('milano_2019/calendar.csv') 
cal_feb = pd.read_csv('milano_2019/calendar-2.csv')
cal_mar = pd.read_csv('milano_2019/calendar-3.csv')
cal_apr = pd.read_csv('milano_2019/calendar-4.csv')
cal_may = pd.read_csv('milano_2019/calendar-6.csv')
cal_jun = pd.read_csv('milano_2019/calendar-5.csv')
cal_jul = pd.read_csv('milano_2019/calendar-7.csv')
cal_aug = pd.read_csv('milano_2019/calendar-9.csv')
cal_sep = pd.read_csv('milano_2019/calendar-12.csv')
cal_oct = pd.read_csv('milano_2019/calendar-8.csv')
cal_nov = pd.read_csv('milano_2019/calendar-10.csv')
cal_dec = pd.read_csv('milano_2019/calendar-11.csv')

