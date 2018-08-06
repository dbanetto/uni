// Q1 A) 1)

(Musician WHERE Mus_Type = 'Conductor') { Mus_Name }


// Q1 A) 2)

((Musician JOIN (Played_By JOIN Instrument)) WHERE (Mus_Type = "Singer")) {Mus_Name, Inst_Name}


// Q1 A) 3)

((((Musician JOIN (Played_By JOIN Instrument)) WHERE (Inst_Name = 'Piano')) UNION
((Musician JOIN (Played_By JOIN Instrument)) WHERE (Inst_Name = 'Violin')))
MINUS
((Musician JOIN (Played_By JOIN Instrument)) WHERE (Inst_Name = 'Guitar'))) {Mus_Name}

// Q1 B) 1)


((Musician) {MusicianId, Mus_Name}) MINUS ((Musician JOIN Played_By) {MusicianId, Mus_Name})


// Q1 B) 2)

SUMMARIZE (Instrument JOIN Played_By) PER ((Instrument JOIN Played_By) {InstrumentId, Inst_Name} )
ADD (COUNT() AS C)
