package design_problems.music_recommender;

import java.util.List;

public class Song {
    String id;
    String name;
    SongCatalog catalog;
    List<Singer> singer;

    public SongCatalog getCatalog() {
        return catalog;
    }

    public List<Singer> getSinger() {
        return singer;
    }
}
