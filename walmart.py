 #!/usr/bin/env python

"""Kaggle Walmart recruiting competition 2014-02-20 to 2014-05-05.
   Reads training and test sets and writes a submission file
   (submission.csv).
   The working directory ~/walmart is expected to contain the input files.
"""

import os
import pandas as pd
from datetime import datetime

os.chdir('/home/jfk/walmart')

train = pd.read_csv('train.csv')  # Training data covers 2010-02-05 to 2012-11-01
test = pd.read_csv('test.csv')    # Test data covers 2012-11-02 to 2013-07-26
assert(len(train) == 421570)
assert(len(test) == 115064)

train['dt'] = [datetime.strptime(d, "%Y-%m-%d") for d in train.Date]
train['yr'] = [d.year for d in train.dt]
train['wk'] = [int(d.strftime("%U")) + 1 for d in train.dt]
train.rename(columns = {'Weekly_Sales': 'sales'}, inplace = True)

test['dt'] = [datetime.strptime(d, "%Y-%m-%d") for d in test.Date]
test['yr'] = [d.year for d in test.dt]
test['wk'] = [int(d.strftime("%U")) + 1 for d in test.dt]
test['prior_yr'] = test.yr - 1

weeks_map = {47:48, 48:49, 14:15}
test['mapped_wk'] = [weeks_map.get(num, num) for num in test.wk]

ans = pd.merge(test, train, how = 'left',
               left_on = ['Store', 'Dept', 'prior_yr', 'mapped_wk'],
               right_on =  ['Store', 'Dept', 'yr', 'wk'])





"""merge(left, right, how='left', on=None, left_on=None, right_on=None,
      left_index=False, right_index=False, sort=True,
      suffixes=('_x', '_y'), copy=True)"""

#train.set_index(['Store', 'Dept', 'yr', 'wk'], inplace = True)
#train.sortlevel(0, inplace = True)
