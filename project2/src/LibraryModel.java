/*
 * LibraryModel.java
 * Author: David Barnett
 */

import java.sql.*;

import javax.swing.*;
import java.sql.Connection;
import java.sql.SQLException;

// canary for java is configured properly
import org.postgresql.Driver;

public class LibraryModel {

    // For use in creating dialogs and making them modal
    private JFrame dialogParent;
    private final String userId;
    private final String password;

    static {
        try {
            Class.forName(org.postgresql.Driver.class.getName());
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    private String getUrl() {
        return "jdbc:postgresql://depot.ecs.vuw.ac.nz/" + userId + "_jdbc";
    }

    private Connection getConnection() throws SQLException {
        return DriverManager.getConnection(getUrl(), userId, password);
    }

    public LibraryModel(JFrame parent, String userid, String password) {
        this.dialogParent = parent;
        this.password = password;
        this.userId  = userid;
    }

    public String bookLookup(int isbn) {
        Connection conn = null;
        StringBuilder result = new StringBuilder();

        try {
            conn = getConnection();

            Statement stmt = conn.createStatement();

            ResultSet results = stmt.executeQuery("SELECT * FROM book WHERE isbn = " + isbn);

            while(results.next()) {

                int ISBN = results.getInt("isbn");
                String title = results.getString("title");
                int edition = results.getInt("edition_no");
                int copies = results.getInt("numofcop");
                int left = results.getInt("numleft");


                result.append(ISBN).append('\t');
                result.append(title).append('\t');
                result.append(edition).append('\t');
                result.append(copies).append('\t');
                result.append(left);
            }

        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            if (conn != null) {
                try {
                    conn.close();
                } catch (SQLException e) {
                    e.printStackTrace();
                }
            }
        }
        return result.toString();
    }

    public String showCatalogue() {
        return "Show Catalogue Stub";
    }

    public String showLoanedBooks() {
        return "Show Loaned Books Stub";
    }

    public String showAuthor(int authorID) {
        return "Show Author Stub";
    }

    public String showAllAuthors() {
        return "Show All Authors Stub";
    }

    public String showCustomer(int customerID) {
        return "Show Customer Stub";
    }

    public String showAllCustomers() {
        return "Show All Customers Stub";
    }

    public String borrowBook(int isbn, int customerID,
            int day, int month, int year) {
        return "Borrow Book Stub";
    }

    public String returnBook(int isbn, int customerid) {
        return "Return Book Stub";
    }

    public void closeDBConnection() {
    }

    public String deleteCus(int customerID) {
        return "Delete Customer";
    }

    public String deleteAuthor(int authorID) {
        return "Delete Author";
    }

    public String deleteBook(int isbn) {
        return "Delete Book";
    }
}
