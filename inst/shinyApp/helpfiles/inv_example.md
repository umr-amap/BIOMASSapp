This dataset contains 4 of the 12 plots of `Petit Plateau' permanent plots fifth census, 2012, Nouragues forestTree dataset (French Guiana). For educational purposes, **158 simulated heights trees have been added** in the dataset, as well as some virtual trees. Dead trees have been removed.

Reference: `Petit Plateau' permanent plots fifth census, 2012, Nouragues forest, <a href="https://dataverse.cirad.fr/dataset.xhtml?persistentId=doi:10.18167/DVN1/TZ1RL9">link here</a>

[https://dataverse.cirad.fr/dataset.xhtml?persistentId=doi:10.18167/DVN1/TZ1RL9](https://dataverse.cirad.fr/dataset.xhtml?persistentId=doi:10.18167/DVN1/TZ1RL9){:target="_blank"}.

<br>

#### **How to use this exemple dataset ?**

<br>

- In the **Forest inventory file box**: you need to specify that the dataset contains several plots and that the `Plot` column contains the plot IDs

<br>

- In the **Required parameters box**: 
  - specify that the column corresponding to the **Diameter** of the trees is the `D` column
  - as no wood density is available, specify that the columns corresponding to the **Genus** and **Species** are respectively the `Genus` and `Species` columns
  - for the **Height** section, you can either: 
    - specify that you have the **height of some trees in the same dataset** and then select the `H` column and leave the plot IDs column `unselected` (as there are only 30 trees per plot, this is insufficient to build one model for each plot)
    - specify that **no height measurements** are available. In this case, when you want to estimate the height of the trees in the next sections, you won't be able to use a local height-diameter model. You will then have to estimate these heights based on the other two methods using plot coordinates.

<br>

- In the **Geographic coordinates box**, you can either:
  - download the exemple of the coordinates dataset and select it after specifying that you have **the coordinates of the plots in another dataset** (2nd option, and then fill Latitude = Lat, Longitude = Long, Plots IDs = Plot)
  - specify manually the coordinates of the region (3rd option) with Latitude = 4.0849 and Longitude = -52.6844
  - specify that you do not have any coordinates (4th option) if you have specified the height of some trees in the Height section. In this case, when you want to estimate tree heights later on, you can only use the estimation method based on the local height-diameter model.


