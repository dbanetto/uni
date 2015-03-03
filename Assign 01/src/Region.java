import java.io.*;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Set;

public class Region {
    private static HashMap<Integer, Region> regions;
    private static Boolean regions_loaded;

    public final String RegionName;
    public final int CountryId;

    private Region(String RegionName, int CountryId) {
        this.RegionName = RegionName;
        this.CountryId = CountryId;
    }

    public static void LoadRegions (File region) {
        regions = new HashMap<>();
        assert (region.isFile());
        assert (region.canRead());

        // Load Regions
        try {
            BufferedReader reader = new BufferedReader(new FileReader(region));
            int id = 0, countryId = 0;
            String regionName = "";
            String line;
            while ((line = reader.readLine()) != null) {
                String[] regionParts = line.split("=");
                if (regionParts[0].startsWith("Region")) {
                    id = Integer.parseInt(regionParts[0].substring("Region".length()));
                    regionName = regionParts[1];
                } else if (regionParts[0].startsWith("CountryId")) {
                    int id_country = Integer.parseInt(regionParts[0].substring("CountryId".length()));
                    if (id_country != id) {
                        System.out.println("IDs for CountryId and Region do not match. Skipping");
                        continue;
                    }
                    countryId = Integer.parseInt(regionParts[1]);
                    regions.put(id, new Region(regionName, countryId));
                } else {
                    System.out.println("Do not understand " + regionParts[0] + ". Skipping");
                    continue;
                }
            }

        } catch (FileNotFoundException e) {
            System.err.println("Could not find " + region.getName() +
                    "\n" + e.toString());
        } catch (IOException e) {
            System.err.println("IO Exception while operating on " + region.getName() +
                    "\n" + e.toString());
        }
        regions_loaded = true;
    }

    @Override
    public boolean equals(Object ob) {
        if (ob instanceof  Region) {
            Region re = (Region)(ob);
            return (re.RegionName.equals(this.RegionName) &&
                    re.CountryId == this.CountryId);
        }
        return false;
    }

    public static Region GetRegion(int id) {
        if (!regions_loaded) {
            // FIXME: Should throw error?
            return null;
        }
        if (!regions.containsKey(id)) {
            return null;
        }
        return regions.get(id);
    }
}
