package design_problems.food_delivery_system.utils;

import design_problems.food_delivery_system.domain.Location;

public interface Utils {
    double EARTH_RADIUS_KM = 6371.0;
    double TO_RADIANS = Math.PI / 180.0;

    static double getDistance(Location a, Location b) {
        double lat1 = a.getLatitude() * TO_RADIANS;
        double lon1 = a.getLongitude() * TO_RADIANS;
        double lat2 = b.getLatitude() * TO_RADIANS;
        double lon2 = b.getLongitude() * TO_RADIANS;

        double dLat = lat2 - lat1;
        double dLon = lon2 - lon1;

        double sinDLat = Math.sin(dLat / 2);
        double sinDLon = Math.sin(dLon / 2);

        double aa = sinDLat * sinDLat +
                Math.cos(lat1) * Math.cos(lat2) *
                        sinDLon * sinDLon;

        double c = 2 * Math.atan2(Math.sqrt(aa), Math.sqrt(1 - aa));

        return EARTH_RADIUS_KM * c;
    }
}
