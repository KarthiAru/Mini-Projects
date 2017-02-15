# Import packages
import pandas as pd
import numpy as np
import math
from haversine import haversine
from tqdm import tqdm
pd.set_option('precision', 11)

def planter_harvester_join(planter, harvester):
    # Import data
	print("\n1. Reading Files...")
	planter = pd.read_csv(planter)
	harvester = pd.read_csv(harvester)
	planter = planter.sort_values(['long', 'lat']).reset_index(drop=True)
	harvester = harvester.sort_values(['long', 'lat']).reset_index(drop=True)

	# Bounding box & field dimensions
	b_long = min(planter['long'].min(), harvester['long'].min()), max(planter['long'].max(), harvester['long'].max())
	b_lat = min(planter['lat'].min(), harvester['lat'].min()), max(planter['lat'].max(), harvester['lat'].max())
	length = round(haversine((b_lat[0], b_long[0]), (b_lat[1], b_long[0])) * 1000, 1)
	breadth = round(haversine((b_lat[0], b_long[0]), (b_lat[0], b_long[1])) * 1000, 1)
	lat_pm = (b_lat[1] - b_lat[0]) / length
	long_pm = (b_long[1] - b_long[0]) / breadth

	# Create Gridlines
	print("\n2. Creating Gridlines...")

	def myrange(start, step, count):
		return [start + i * step for i in range(count)]
	# vertical gridlines
	nx = math.ceil((breadth + (long_pm * 5)) / 10)
	x = myrange(b_long[0] - (long_pm * 5), (long_pm * 10), count=nx + 2)
	# horizontal gridlines
	ny = math.ceil((breadth + (lat_pm * 5)) / 10)
	y = myrange(b_lat[0] - (lat_pm * 5), (lat_pm * 10), count=ny + 2)

	# Create boundaries for Grids
	dfx = pd.DataFrame(x)
	dfx.columns = ['value']
	dfx['value1'] = dfx.shift(-1) - 1e-10
	dfx['groupX'] = range(0, len(dfx['value']), 1)
	dfx = dfx.dropna()

	dfy = pd.DataFrame(y)
	dfy.columns = ['value']
	dfy['value1'] = dfy.shift(-1) - 1e-10
	dfy['groupY'] = range(0, len(dfy['value']), 1)
	dfy = dfy.dropna()

	# Assign Grid Membership
	import itertools
	def expand_grid(data_dict):
		rows = itertools.product(*data_dict.values())
		return pd.DataFrame.from_records(rows, columns=data_dict.keys())
	
	group = expand_grid({'groupX': range(0, dfx.count()[0], 1),'groupY': range(0, dfy.count()[0], 1)})
	group['value'] = group['groupY'].map(str) + "-" + group['groupX'].map(str)
	group['group'] = range(0, group.count()[0], 1)
	group.drop(['groupX', 'groupY'], axis=1, inplace=True)

	# Map planter data to grids
	print("\n3. Assigning Grids to Planter...")
	# vertical grids
	planter_grid = planter.copy()
	planter_grid['groupX'] = np.piecewise(np.zeros(planter_grid.count()[0]),
                                      [(planter_grid['long'].values >= start) & (planter_grid['long'].values <= end) for start, end in zip(dfx['value'].values, dfx['value1'].values)], dfx['groupX'].values).astype(int)
	# horizontal grids
	planter_grid['groupY'] = np.piecewise(np.zeros(planter.count()[0]),
                                      [(planter_grid['lat'].values >= start) & (planter_grid['lat'].values <= end) for start, end in zip(dfy['value'].values, dfy['value1'].values)], dfy['groupY'].values).astype(int)

	# grid membership
	planter_grid['value'] = planter_grid['groupY'].map(str) + "-" + planter_grid['groupX'].map(str)
	planter_grid.drop(['groupX', 'groupY'], axis=1, inplace=True)
	planter_grid = pd.merge(planter_grid, group, on='value')
	planter_grid = planter_grid.sort_values(['long', 'lat']).reset_index(drop=True)

	# Map harvester data to grids
	print("\n4. Assigning Grids to Harvester...")
	# vertical grids
	harvester_grid = harvester.copy()
	harvester_grid['groupX'] = np.piecewise(np.zeros(harvester_grid.count()[0]),
                                        [(harvester_grid['long'].values >= start) & (harvester_grid['long'].values <= end) for start, end in zip(dfx['value'].values, dfx['value1'].values)], dfx['groupX'].values).astype(int)
	# horizontal grids
	harvester_grid['groupY'] = np.piecewise(np.zeros(harvester_grid.count()[0]),
                                        [(harvester_grid['lat'].values >= start) & (harvester_grid['lat'].values <= end) for start, end in zip(dfy['value'].values, dfy['value1'].values)], dfy['groupY'].values).astype(int)

	# grid membership
	harvester_grid['value'] = harvester_grid['groupY'].map(str) + "-" + harvester_grid['groupX'].map(str)
	harvester_grid.drop(['groupX', 'groupY'], axis=1, inplace=True)
	harvester_grid = pd.merge(harvester_grid, group, on='value')
	harvester_grid = harvester_grid.sort_values(
    ['long', 'lat']).reset_index(drop=True)

	# Map planter - harvester data
	print("\n5. Joining Planter-Harvester...\n")


	def haversine_vect(x):
		return round(haversine((x['lat'], x['long']), (x['lat1'], x['long1'])) * 1000, 1)
	
	harvester_grid = pd.concat([harvester_grid,
                            pd.DataFrame(columns=['variety', 'seeding_rate', 'seed_spacing', 'speed', 'long1', 'lat1', 'dist'])], axis=1)

	for i in tqdm(range(harvester_grid.count()[0])):
		harvester_gps = harvester_grid.loc[i, ].copy()
		n = harvester_gps['value'].split('-')
		row = int(n[0])
		col = int(n[1])

		# filter the rows from planting - nearest neighbour search
		neighbour = [
		"%s-%s" % (row, col - 1),
		"%s-%s" % (row - 1, col),
		"%s-%s" % (row, col),
		"%s-%s" % (row + 1, col),
		"%s-%s" % (row, col + 1)
		]

		planter_gps = planter_grid[planter_grid['value'].isin(neighbour)].copy().reset_index(drop=True)

		if(planter_gps.count()[0] > 0):
			planter_gps['long1'] = harvester_gps['long']
			planter_gps['lat1'] = harvester_gps['lat']
			# find the distance between geo-coordinates
			planter_gps['dist'] = planter_gps.apply(lambda row: haversine_vect(row), axis=1)
			planter_gps = planter_gps[planter_gps['dist'] == planter_gps['dist'].min()].reset_index(drop=True)
			# map the data back to harvester_grid
			harvester_grid.loc[i, 'variety'] = planter_gps.loc[0, 'variety']
			harvester_grid.loc[i, 'seeding_rate'] = planter_gps.loc[0, 'seeding_rate']
			harvester_grid.loc[i, 'seed_spacing'] = planter_gps.loc[0, 'seed_spacing']
			harvester_grid.loc[i, 'speed'] = planter_gps.loc[0, 'speed']
			harvester_grid.loc[i, 'long1'] = planter_gps.loc[0, 'long1']
			harvester_grid.loc[i, 'lat1'] = planter_gps.loc[0, 'lat1']
			harvester_grid.loc[i, 'dist'] = planter_gps.loc[0, 'dist']

	# Save the file
	print("\n6. Saving file...")
	harvester_grid.to_csv('planter-harvester-python.csv', index=False)

if __name__ == '__main__':
    planter_harvester_join('planting_sample_data.csv','harvest_sample_data.csv')
