# apply a few constraints to donors identified (e.g., via Gower's distance or another technique)
def func(rec, donors, dists, pars, dtAttr):
  
    import numpy as np
  
    # 1. narrow down to donors with the same snowiness category
    snowy = dtAttr.query("id==@rec & tag=='receiver'")['snowy']
    snowy1 = dtAttr.query("id in @donors & tag=='donor'")['snowy']
    ix1 = snowy1.isin(snowy)
    if sum(ix1) > 0:
        dists = np.array(dists)[ix1]
        donors = np.array(donors)[ix1]
  
    # 2. further narrow down to those donors within maximum spatial distance defined
    ix1 = dists <= pars['general']['maxSpaDist']
    if sum(ix1) > 0:
        dists = np.array(dists)[ix1]
        donors = np.array(donors)[ix1]  
  
    # 3. further narrow down based on screening attributes
    for att1 in pars['general']['maxAttrDiff'].keys():
        ix1 = abs(np.array(dtAttr.query("id==@rec & tag=='receiver'")[att1]) - \
            np.array(dtAttr.query("id in @donors & tag=='donor'")[att1])) \
                <= pars['general']['maxAttrDiff'][att1]
        if sum(ix1) > 0:
            dists = np.array(dists)[ix1]
            donors = np.array(donors)[ix1]  
        
    # 4. further narrow down to donors in the same HSG
    hsg = dtAttr.query("id==@rec & tag=='receiver'")['hsg']
    hsg1 = dtAttr.query("id in @donors & tag=='donor'")['hsg']
    ix1 = hsg1.isin(hsg)
    if sum(ix1) > 0:
        dists = np.array(dists)[ix1]
        donors = np.array(donors)[ix1]
  
    return donors, dists
  