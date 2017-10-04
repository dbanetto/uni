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

import static javax.swing.JOptionPane.ERROR_MESSAGE;
import static javax.swing.JOptionPane.showMessageDialog;

public class LibraryModel {

    // For use in creating dialogs and making them modal
    private JFrame dialogParent;
    private final Connection conn;

    static {
        try {
            Class.forName(org.postgresql.Driver.class.getName());
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    private String getUrl(String userId) {
        return "jdbc:postgresql://depot.ecs.vuw.ac.nz/" + userId + "_jdbc";
    }

    private Connection getConnection(String userId, String password) throws SQLException {
        return DriverManager.getConnection(getUrl(userId), userId, password);
    }

    public LibraryModel(JFrame parent, String userid, String password) throws SQLException {
        this.dialogParent = parent;
        this.conn = getConnection(userid, password);
    }

    public String bookLookup(int isbn) {
        StringBuilder builder = new StringBuilder();

        try {
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
        StringBuilder builder = new StringBuilder();

        try {
            Statement stmt = conn.createStatement();

            ResultSet query = stmt.executeQuery("SELECT * FROM book");

            buildBooks(query, builder);

        } catch (SQLException e) {
            builder.append("Error occurred while looking up catalogue").append('\n');
            builder.append(e.toString()).append('\n');
            e.printStackTrace();
        }
        return builder.toString();
    }

    public String showLoanedBooks() {
        StringBuilder builder = new StringBuilder();

        try {
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
                int customerId = query.getInt("customerid");


                builder.append(ISBN).append('\t');
                builder.append(dueDate).append('\t');
                builder.append(customerId);
                builder.append('\n');
            }

        } catch (SQLException e) {
            builder.append("Error occurred while looking up catalogue").append('\n');
            builder.append(e.toString()).append('\n');
            e.printStackTrace();
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
        StringBuilder builder = new StringBuilder();

        try {
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
        }
        return builder.toString();
    }

    public String showAllAuthors() {
        StringBuilder builder = new StringBuilder();

        try {
            Statement stmt = conn.createStatement();

            ResultSet query = stmt.executeQuery("SELECT * FROM author");

            buildAuthor(query, builder);

        } catch (SQLException e) {
            builder.append("Error occurred while looking up authors").append('\n');
            builder.append(e.toString()).append('\n');
            e.printStackTrace();
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
        StringBuilder builder = new StringBuilder();

        try {
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
        }
        return builder.toString();
    }

    public String showAllCustomers() {
        StringBuilder builder = new StringBuilder();

        try {
            Statement stmt = conn.createStatement();

            ResultSet query = stmt.executeQuery("SELECT * FROM customer");

            buildCustomer(query, builder);

        } catch (SQLException e) {
            builder.append("Error occurred while looking up customers").append('\n');
            builder.append(e.toString()).append('\n');
            e.printStackTrace();
        }
        return builder.toString();
    }

    public String borrowBook(int isbn, int customerID,
            int day, int month, int year) {

        StringBuilder builder = new StringBuilder();

        try {
            conn.setAutoCommit(false);

            Statement stmt = conn.createStatement();

            ResultSet customer = stmt.executeQuery("SELECT 1 FROM customer WHERE customerid = " + customerID + " FOR UPDATE");
            if (!customer.isBeforeFirst()) {
                throw new SQLException("customer with id " + customerID + " does not exist");
            }

            ResultSet book = stmt.executeQuery("SELECT numofcop, numleft FROM book WHERE isbn = " + isbn + " FOR UPDATE");
            int numberLeft;
            if (!book.isBeforeFirst()) {
                throw new SQLException("book with ISBN " + isbn + " does not exist");
            } else {
                book.next();
                numberLeft = book.getInt("numleft") - 1;
                if (numberLeft < 0) {
                    throw new SQLException(isbn + " does not have enough copies available to borrow");
                }
            }

            PreparedStatement borrowInsert = conn.prepareStatement("INSERT INTO cust_book VALUES (? , ?, ?)");
            borrowInsert.setInt(1, isbn);
            borrowInsert.setDate(2, Date.valueOf(year + "-" + month + "-" + day));
            borrowInsert.setInt(3, customerID);
            int borrowResult = borrowInsert.executeUpdate();
            if (borrowResult == 0) {
                throw new SQLException("Failed to insert into cust_book");
            }

            showMessageDialog(dialogParent, "Confirm to update the Book table",
                    "Confirm Update", JOptionPane.INFORMATION_MESSAGE);

            PreparedStatement bookUpdate = conn.prepareStatement("UPDATE book SET numleft = ? WHERE isbn = ?");
            bookUpdate.setInt(1, numberLeft);
            bookUpdate.setInt(2, isbn);
            int bookUpdateResult = bookUpdate.executeUpdate();
            if (bookUpdateResult == 0) {
                throw new SQLException("Failed to update book count");
            }

            conn.commit();
            builder.append("Successfully recorded customer (").append(customerID)
                    .append(") borrowing ").append(isbn).append(" (ISBN) till ")
                    .append(year).append('-').append(month).append('-').append(day)
                    .append(" there is ").append(numberLeft).append(" copies left.")
                    .append('\n');
        } catch (SQLException e) {
            builder.append("Error occurred while borrowing book").append('\n');
            builder.append(e.getMessage()).append('\n');
        } finally {
            try {
                conn.rollback();
                conn.setAutoCommit(true);
            } catch (SQLException e) {
                e.printStackTrace();
            }
        }
        return builder.toString();
    }

    public String returnBook(int isbn, int customerID) {
        StringBuilder builder = new StringBuilder();

        try {
            conn.setAutoCommit(false);

            Statement stmt = conn.createStatement();

            ResultSet customer = stmt.executeQuery("SELECT 1 FROM customer WHERE customerid = " + customerID + " FOR UPDATE");
            if (!customer.isBeforeFirst()) {
                throw new SQLException("Customer with id " + customerID + " does not exist");
            }

            ResultSet book = stmt.executeQuery("SELECT numleft FROM book WHERE isbn = " + isbn + " FOR UPDATE");
            int numberLeft;
            if (!book.isBeforeFirst()) {
                throw new SQLException("book with ISBN " + isbn + " does not exist");
            } else {
                book.next();
                numberLeft = book.getInt("numleft") + 1;
            }

            PreparedStatement borrowDelete = conn.prepareStatement("DELETE FROM cust_book WHERE customerid = ? AND isbn = ?");
            borrowDelete.setInt(1, customerID);
            borrowDelete.setInt(2, isbn);
            int borrowResult = borrowDelete.executeUpdate();
            if (borrowResult == 0) {
                throw new SQLException("Failed to delete from borrow table");
            }

            PreparedStatement bookUpdate = conn.prepareStatement("UPDATE book SET numleft = ? WHERE isbn = ?");
            bookUpdate.setInt(1, numberLeft);
            bookUpdate.setInt(2, isbn);
            int bookUpdateResult = bookUpdate.executeUpdate();
            if (bookUpdateResult == 0) {
                throw new SQLException("Failed to update book count");
            }

            conn.commit();
            builder.append("Successfully recorded customer (").append(customerID)
                    .append(") returning ").append(isbn).append(" (ISBN)").append('\n');
        } catch (SQLException e) {
            builder.append("Error occurred while returning book").append('\n');
            builder.append(e.getMessage()).append('\n');
        } finally {
            try {
                conn.rollback();
                conn.setAutoCommit(true);
            } catch (SQLException e) {
                e.printStackTrace();
            }
        }
        return builder.toString();
    }

    public void closeDBConnection() {
        if (conn != null) {
            try {
                conn.close();
            } catch (SQLException e) {
                e.printStackTrace();
            }
        }
    }

    public String deleteCustomer(int customerID) {
        StringBuilder builder = new StringBuilder();

        try {
            conn.setAutoCommit(false);

            Statement stmt = conn.createStatement();

            ResultSet customer = stmt.executeQuery("SELECT 1 FROM customer WHERE customerid = " + customerID + " FOR UPDATE");
            if (!customer.isBeforeFirst()) {
                throw new SQLException("Customer with id " + customerID + " does not exist");
            }

            ResultSet book = stmt.executeQuery("SELECT 1 FROM cust_book WHERE customerid = " + customerID);
            if (book.isBeforeFirst()) {
                throw new SQLException("Customer has books borrowed, cannot deleted customer until all books are returned");
            }

            PreparedStatement customerDelete = conn.prepareStatement("DELETE FROM customer WHERE customerid = ?");
            customerDelete.setInt(1, customerID);
            int borrowResult = customerDelete.executeUpdate();
            if (borrowResult == 0) {
                throw new SQLException("Failed to delete from borrow table");
            }

            conn.commit();
            builder.append("Successfully customer deleted customer (").append(customerID)
                    .append(")") .append('\n');
        } catch (SQLException e) {
            builder.append("Error occurred while deleting customer ").append(customerID).append('\n');
            builder.append(e.getMessage()).append('\n');
        } finally {
            try {
                conn.rollback();
                conn.setAutoCommit(true);
            } catch (SQLException e) {
                e.printStackTrace();
            }
        }
        return builder.toString();
    }

    public String deleteAuthor(int authorID) {
        StringBuilder builder = new StringBuilder();

        try {
            conn.setAutoCommit(false);

            Statement stmt = conn.createStatement();

            ResultSet customer = stmt.executeQuery("SELECT 1 FROM author WHERE authorid = " + authorID + " FOR UPDATE");
            if (!customer.isBeforeFirst()) {
                throw new SQLException("Author with id " + authorID + " does not exist");
            }

            ResultSet book = stmt.executeQuery("SELECT 1 FROM book_author WHERE authorid = " + authorID);
            if (book.isBeforeFirst()) {
                throw new SQLException("Author has books, cannot delete author until all books are deleted");
            }

            PreparedStatement customerDelete = conn.prepareStatement("DELETE FROM author WHERE authorid = ?");
            customerDelete.setInt(1, authorID);
            int borrowResult = customerDelete.executeUpdate();
            if (borrowResult == 0) {
                throw new SQLException("Failed to delete from author table");
            }

            conn.commit();
            builder.append("Successfully deleted author (").append(authorID)
                    .append(")") .append('\n');
        } catch (SQLException e) {
            builder.append("Error occurred while deleting author ").append(authorID).append('\n');
            builder.append(e.getMessage()).append('\n');
        } finally {
            try {
                conn.rollback();
                conn.setAutoCommit(true);
            } catch (SQLException e) {
                e.printStackTrace();
            }
        }
        return builder.toString();
    }

    public String deleteBook(int isbn) {
        StringBuilder builder = new StringBuilder();

        try {
            conn.setAutoCommit(false);

            Statement stmt = conn.createStatement();

            ResultSet customer = stmt.executeQuery("SELECT 1 FROM book WHERE isbn = " + isbn + " FOR UPDATE");
            if (!customer.isBeforeFirst()) {
                throw new SQLException("Book with ISBN " + isbn + " does not exist");
            }

            ResultSet book = stmt.executeQuery("SELECT 1 FROM cust_book WHERE isbn = " + isbn);
            if (book.isBeforeFirst()) {
                throw new SQLException("A copy of the book is borrowed, cannot deleted book until all copies are returned");
            }

            PreparedStatement bookDelete = conn.prepareStatement("DELETE FROM book WHERE isbn = ?");
            bookDelete.setInt(1, isbn);
            int borrowResult = bookDelete.executeUpdate();
            if (borrowResult == 0) {
                throw new SQLException("Failed to delete from book table");
            }

            PreparedStatement bookAuthorDelete = conn.prepareStatement("DELETE FROM book_author WHERE isbn = ?");
            bookAuthorDelete.setInt(1, isbn);
            borrowResult = bookAuthorDelete.executeUpdate();

            conn.commit();
            builder.append("Successfully deleted book (").append(isbn).append(")").append('\n');
        } catch (SQLException e) {
            builder.append("Error occurred while deleting book ").append(isbn).append('\n');
            builder.append(e.getMessage()).append('\n');
        } finally {
            try {
                conn.rollback();
                conn.setAutoCommit(true);
            } catch (SQLException e) {
                e.printStackTrace();
            }
        }
        return builder.toString();
    }
}
