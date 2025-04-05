package design_problems.music_recommender;

import java.util.List;

public class PlayList {
    String id;
    String name;
    List<Song> songs;

    public List<Song> getSongs() {
        return songs;
    }
}
