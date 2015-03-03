public enum RoadType {
    Residential(0),
    Collector(1),
    Arterial(2),
    PrincipalHW(3),
    MajorHW(4);

    private final int rank;

    private RoadType(int Rank) {
        this.rank = Rank;
    }

    public int getRank() { return this.rank; }
}
