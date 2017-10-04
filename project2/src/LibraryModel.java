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
        StringBuilder builder = new StringBuilder();

        try {
            conn = getConnection();

            Statement stmt = conn.createStatement();

            ResultSet results = stmt.executeQuery("SELECT * FROM book WHERE isbn = " + isbn);

            if (results.isBeforeFirst()) {
                buildBooks(results, builder);
            } else {
                builder.append("No book with ISBN ").append(isbn).append(" found.").append('\n');
            }

        } catch (SQLException e) {
            builder.append("Error occurred while looking up ").append(isbn).append('\n');
            builder.append(e.toString()).append('\n');
            e.printStackTrace();
        } finally {
            if (conn != null) {
                try {
                    conn.close();
                } catch (SQLException e) {
                    builder.append("Error occurred while closing connection").append('\n');
                    builder.append(e.toString()).append('\n');
                    e.printStackTrace();
                }
            }
        }
        return builder.toString();
    }

    private void buildBooks(ResultSet bookQuery, StringBuilder builder) throws SQLException {

        // header
        builder.append("ISBN").append('\t');
        builder.append("Title").append('\t');
        builder.append("Edition").append('\t');
        builder.append("Number of Copies").append('\t');
        builder.append("Number Left");
        builder.append('\n');

        while(bookQuery.next()) {

            int ISBN = bookQuery.getInt("isbn");
            String title = bookQuery.getString("title");
            int edition = bookQuery.getInt("edition_no");
            int copies = bookQuery.getInt("numofcop");
            int left = bookQuery.getInt("numleft");


            builder.append(ISBN).append('\t');
            builder.append(title).append('\t');
            builder.append(edition).append('\t');
            builder.append(copies).append('\t');
            builder.append(left);
            builder.append('\n');
        }
    }

    public String showCatalogue() {
        Connection conn = null;
        StringBuilder builder = new StringBuilder();

        try {
            conn = getConnection();

            Statement stmt = conn.createStatement();

            ResultSet query = stmt.executeQuery("SELECT * FROM book");

            buildBooks(query, builder);

        } catch (SQLException e) {
            builder.append("Error occurred while looking up catalogue").append('\n');
            builder.append(e.toString()).append('\n');
            e.printStackTrace();
        } finally {
            if (conn != null) {
                try {
                    conn.close();
                } catch (SQLException e) {
                    builder.append("Error occurred while closing connection").append('\n');
                    builder.append(e.toString()).append('\n');
                    e.printStackTrace();
                }
            }
        }
        return builder.toString();
    }

    public String showLoanedBooks() {
        Connection conn = null;
        StringBuilder builder = new StringBuilder();

        try {
            conn = getConnection();

            Statement stmt = conn.createStatement();

            ResultSet query = stmt.executeQuery("SELECT * FROM cust_book");

            // header
            builder.append("ISBN").append('\t');
            builder.append("Due Date").append('\t');
            builder.append("Customer Id");
            builder.append('\n');

            while(query.next()) {

                int ISBN = query.getInt("isbn");
                String dueDate = query.getString("duedate");
                int customerId = query.getInt("customer_id");


                builder.append(ISBN).append('\t');
                builder.append(dueDate).append('\t');
                builder.append(customerId);
                builder.append('\n');
            }

        } catch (SQLException e) {
            builder.append("Error occurred while looking up catalogue").append('\n');
            builder.append(e.toString()).append('\n');
            e.printStackTrace();
        } finally {
            if (conn != null) {
                try {
                    conn.close();
                } catch (SQLException e) {
                    builder.append("Error occurred while closing connection").append('\n');
                    builder.append(e.toString()).append('\n');
                    e.printStackTrace();
                }
            }
        }
        return builder.toString();
    }

    private void buildAuthor(ResultSet author, StringBuilder builder) throws SQLException {
        // header
        builder.append("Author Id").append('\t');
        builder.append("Name").append('\t');
        builder.append("Surname");
        builder.append('\n');

        while(author.next()) {

            int id = author.getInt("authorid");
            String name = author.getString("name");
            String surname = author.getString("surname");


            builder.append(id).append('\t');
            builder.append(name).append('\t');
            builder.append(surname);
            builder.append('\n');
        }

    }

    public String showAuthor(int authorID) {
        Connection conn = null;
        StringBuilder builder = new StringBuilder();

        try {
            conn = getConnection();

            Statement stmt = conn.createStatement();

            ResultSet query = stmt.executeQuery("SELECT * FROM author WHERE authorid = " + authorID);

            if (query.isBeforeFirst()) {
                buildAuthor(query, builder);
            } else {
                builder.append("No author found with id ").append(authorID).append('\n');
            }

        } catch (SQLException e) {
            builder.append("Error occurred while looking up author ").append(authorID).append('\n');
            builder.append(e.toString()).append('\n');
            e.printStackTrace();
        } finally {
            if (conn != null) {
                try {
                    conn.close();
                } catch (SQLException e) {
                    builder.append("Error occurred while closing connection").append('\n');
                    builder.append(e.toString()).append('\n');
                    e.printStackTrace();
                }
            }
        }
        return builder.toString();
    }

    public String showAllAuthors() {
        Connection conn = null;
        StringBuilder builder = new StringBuilder();

        try {
            conn = getConnection();

            Statement stmt = conn.createStatement();

            ResultSet query = stmt.executeQuery("SELECT * FROM author");

            buildAuthor(query, builder);

        } catch (SQLException e) {
            builder.append("Error occurred while looking up authors").append('\n');
            builder.append(e.toString()).append('\n');
            e.printStackTrace();
        } finally {
            if (conn != null) {
                try {
                    conn.close();
                } catch (SQLException e) {
                    builder.append("Error occurred while closing connection").append('\n');
                    builder.append(e.toString()).append('\n');
                    e.printStackTrace();
                }
            }
        }
        return builder.toString();
    }

    private void buildCustomer(ResultSet customer, StringBuilder builder) throws SQLException {
        // header
        builder.append("Customer Id").append('\t');
        builder.append("Last Name").append('\t');
        builder.append("First Name").append('\t');
        builder.append("City");
        builder.append('\n');

        while(customer.next()) {

            int id = customer.getInt("customerid");
            String lname = customer.getString("l_name");
            String fname = customer.getString("f_name");
            String city = customer.getString("city");


            builder.append(id).append('\t');
            builder.append(lname).append('\t');
            builder.append(fname).append('\t');
            builder.append(city);
            builder.append('\n');
        }
    }

    public String showCustomer(int customerID) {
        Connection conn = null;
        StringBuilder builder = new StringBuilder();

        try {
            conn = getConnection();

            Statement stmt = conn.createStatement();

            ResultSet query = stmt.executeQuery("SELECT * FROM customer WHERE customerid = " + customerID);

            if (query.isBeforeFirst()) {
                buildCustomer(query, builder);
            } else {
                builder.append("No customer found with id ").append(customerID).append('\n');
            }

        } catch (SQLException e) {
            builder.append("Error occurred while looking up customer ").append(customerID).append('\n');
            builder.append(e.toString()).append('\n');
            e.printStackTrace();
        } finally {
            if (conn != null) {
                try {
                    conn.close();
                } catch (SQLException e) {
                    builder.append("Error occurred while closing connection").append('\n');
                    builder.append(e.toString()).append('\n');
                    e.printStackTrace();
                }
            }
        }
        return builder.toString();
    }

    public String showAllCustomers() {
        Connection conn = null;
        StringBuilder builder = new StringBuilder();

        try {
            conn = getConnection();

            Statement stmt = conn.createStatement();

            ResultSet query = stmt.executeQuery("SELECT * FROM customer");

            buildCustomer(query, builder);

        } catch (SQLException e) {
            builder.append("Error occurred while looking up customers").append('\n');
            builder.append(e.toString()).append('\n');
            e.printStackTrace();
        } finally {
            if (conn != null) {
                try {
                    conn.close();
                } catch (SQLException e) {
                    builder.append("Error occurred while closing connection").append('\n');
                    builder.append(e.toString()).append('\n');
                    e.printStackTrace();
                }
            }
        }
        return builder.toString();
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
