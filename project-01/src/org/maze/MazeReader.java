package org.maze;

import java.io.*;
import java.util.Scanner;

public class MazeReader {
    public static Maze fromFile(File mazeFile) throws IOException {
        FileReader stream = new FileReader(mazeFile);
        BufferedReader reader = new BufferedReader(stream);

        Square[][] grid;

        Scanner settingsScanner = new Scanner(reader.readLine());

        int size = settingsScanner.nextInt();
        int startY = settingsScanner.nextInt();
        int startX = settingsScanner.nextInt();
        System.out.printf("Size: %d, Entry: (%d, %d)\n", size, startX, startY);

        grid = new Square[size][size];

        int x = 0, y = 0;

        while (reader.ready()) {
            String line = reader.readLine();
            for (char ele : line.toCharArray()) {
                grid[y][x] = fromCharacter(ele);
                x++;
            }
            y++;
            x = 0;
            System.out.println(line);
        }

        // link up all the squares
        for (int r = 0; r < size; r++) {
            for (int c = 0; c < size; c++) {
                Square current = grid[r][c];
                Square north = null, south = null, west = null, east = null;
                if (r - 1 >= 0) {
                    north = grid[r - 1][c];
                }
                if (r + 1 < size) {
                    south = grid[r + 1][c];
                }
                if (c - 1 >= 0) {
                    west = grid[r][c - 1];
                }
                if (c + 1 < size) {
                    east = grid[r][c + 1];
                }
                current.setNeighbours(north, east, south, west);
            }
        }

        return new Maze(grid, startX, startY);
    }

    private static Square fromCharacter(char tile) {
        switch (tile) {
            case 'X':
                return new Square(Tile.WALL);
            case ' ':
                return new Square(Tile.SPACE);
            default:
                throw new IllegalArgumentException("Unexpected tile: " + tile);
        }
    }
}
