package design_problems.music_recommender;

import java.util.List;

public class User {
    String userId;
    String name;
    String email;
    String password;

    List<PlayList> playLists;
    List<Song> likedSongs;

    public List<PlayList> getPlayLists() {
        return playLists;
    }
}
