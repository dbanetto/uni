/*
 * LibraryModel.java
 * Author: David Barnett
 */

import javax.swing.*;
import java.sql.*;

import static javax.swing.JOptionPane.showMessageDialog;

public class LibraryModel {

    // Runs before first instantiation of the LibraryModel class
    static {
        try {
            Class.forName(org.postgresql.Driver.class.getName());
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    private final Connection conn;
    // For use in creating dialogs and making them modal
    private JFrame dialogParent;

    public LibraryModel(JFrame parent, String userid, String password) throws SQLException {
        this.dialogParent = parent;
        this.conn = getConnection(userid, password);
    }

    private String getUrl(String userId) {
        return "jdbc:postgresql://depot.ecs.vuw.ac.nz/" + userId + "_jdbc";
    }

    private Connection getConnection(String userId, String password) throws SQLException {
        return DriverManager.getConnection(getUrl(userId), userId, password);
    }

    public String bookLookup(int isbn) {
        StringBuilder builder = new StringBuilder();

        try {
            PreparedStatement stmt = conn.prepareStatement("SELECT * FROM book WHERE isbn = ?");
            stmt.setInt(1, isbn);
            ResultSet results = stmt.executeQuery();

            builder.append("Book\n");

            if (results.isBeforeFirst()) {
                buildBooks(results, builder);
            } else {
                builder.append("\tNo book with ISBN ").append(isbn).append(" found.").append('\n');
            }

        } catch (SQLException e) {
            builder.append("Error occurred while looking up ").append(isbn).append('\n');
            builder.append(e.toString()).append('\n');
            e.printStackTrace();
        }
        return builder.toString();
    }

    /**
     * Builds a table of books with authors
     *
     * @param bookQuery result of a book query
     * @param builder where the table is built to
     * @throws SQLException
     */
    private void buildBooks(ResultSet bookQuery, StringBuilder builder) throws SQLException {

        while (bookQuery.next()) {

            int ISBN = bookQuery.getInt("isbn");
            String title = bookQuery.getString("title");
            int edition = bookQuery.getInt("edition_no");
            int copies = bookQuery.getInt("numofcop");
            int left = bookQuery.getInt("numleft");

            // build the authors list
            StringBuilder authorList = new StringBuilder();
            PreparedStatement authorsStmt = conn.prepareStatement("SELECT surname FROM book_author natural join author where isbn = ? ORDER BY authorseqno");
            authorsStmt.setInt(1, ISBN);
            ResultSet authors = authorsStmt.executeQuery();

            while (authors.next()) {
                if (!authors.isFirst()) {
                    authorList.append(", ");
                }
                authorList.append(authors.getString("surname").trim());
            }

            builder.append(ISBN).append(": ").append(title).append('\n');
            builder.append('\t')
                    .append("Edition: ").append(edition)
                    .append(" - Number of Copies: ").append(copies)
                    .append(" - Copies left: ").append(left)
                    .append('\n');
            builder.append('\t').append("Authors: ").append(authorList.toString())
                    .append('\n');
        }
    }

    public String showCatalogue() {
        StringBuilder builder = new StringBuilder();

        try {
            Statement stmt = conn.createStatement();

            ResultSet query = stmt.executeQuery("SELECT * FROM book ORDER BY isbn DESC");

            builder.append("Book Catalogue\n");
            buildBooks(query, builder);

            builder.append('\n');
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

            ResultSet query = stmt.executeQuery("SELECT isbn FROM cust_book GROUP BY isbn");

            builder.append("Loaned books\n");

            if (!query.isBeforeFirst()) {
                builder.append("\t (no loaned books)\n");
            }

            while (query.next()) {

                int ISBN = query.getInt("isbn");

                PreparedStatement book = conn.prepareStatement("SELECT * FROM book WHERE isbn = ?");
                book.setInt(1, ISBN);
                ResultSet results = book.executeQuery();

                buildBooks(results, builder);

                builder.append("\tBorrowers:\n");

                PreparedStatement borrowers = conn.prepareStatement("SELECT * FROM customer NATURAL JOIN cust_book WHERE isbn = ?");
                borrowers.setInt(1, ISBN);
                ResultSet borrow = borrowers.executeQuery();

                while (borrow.next()) {
                    int customerId = borrow.getInt("customerid");
                    String lastName = borrow.getString("l_name");
                    String firstName = borrow.getString("f_name");
                    String city = borrow.getString("city");
                    city = city == null ? "(no city)" : city;
                    Date duedate = borrow.getDate("duedate");


                    builder.append("\t\t").append(customerId).append(": ")
                            .append(firstName.trim()).append(' ').append(lastName.trim())
                            .append(" - ").append(city)
                            .append(" due on ").append(duedate.toString())
                            .append('\n');
                }


                builder.append('\n');
            }

        } catch (SQLException e) {
            builder.append("Error occurred while looking up catalogue").append('\n');
            builder.append(e.toString()).append('\n');
            e.printStackTrace();
        }
        return builder.toString();
    }

    /**
     * Builds an author table to be displayed
     *
     * @param author result of an author query
     * @param builder where the table is built to
     * @throws SQLException
     */
    private void buildAuthor(ResultSet author, StringBuilder builder) throws SQLException {
        while (author.next()) {

            int id = author.getInt("authorid");
            String name = author.getString("name");
            String surname = author.getString("surname");


            builder.append('\t').append(id).append(": ")
                    .append(name.trim()).append(", ")
                    .append(surname)
                    .append('\n');
        }

    }

    public String showAuthor(int authorID) {
        StringBuilder builder = new StringBuilder();

        try {

            PreparedStatement stmt = conn.prepareStatement("SELECT * FROM author WHERE authorid = ?");
            stmt.setInt(1, authorID);

            ResultSet query = stmt.executeQuery();

            builder.append("Author\n");

            if (query.isBeforeFirst()) {

                query.next();

                builder.append('\t').append(query.getInt("authorid"))
                        .append(" - ")
                        .append(query.getString("name").trim())
                        .append(query.getString("surname").trim())
                        .append('\n');

                PreparedStatement booksWritten = conn.prepareStatement("SELECT isbn, title FROM book NATURAL JOIN book_author WHERE authorid = ?");
                booksWritten.setInt(1, authorID);

                ResultSet written = booksWritten.executeQuery();

                if (written.isBeforeFirst()) {
                    builder.append("\tBooks written:\n");
                    while (written.next()) {
                        int isbn = written.getInt("isbn");
                        String title = written.getString("title").trim();

                        builder.append("\t\t").append(isbn).append(" - ").append(title).append('\n');
                    }
                } else {
                    builder.append("\t(no books written)").append('\n');
                }

            } else {
                builder.append("\tNo author found with id ").append(authorID).append('\n');
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

            builder.append("Authors\n");

            buildAuthor(query, builder);

        } catch (SQLException e) {
            builder.append("Error occurred while looking up authors").append('\n');
            builder.append(e.toString()).append('\n');
            e.printStackTrace();
        }
        return builder.toString();
    }

    /**
     * Builds a customer table to be displayed
     *
     * @param customer result of an customer query
     * @param builder where the table is built to
     * @throws SQLException
     */
    private void buildCustomer(ResultSet customer, StringBuilder builder) throws SQLException {

        while (customer.next()) {

            int id = customer.getInt("customerid");
            String lname = customer.getString("l_name").trim();
            String fname = customer.getString("f_name").trim();
            String city = customer.getString("city");
            city = city == null ? "(no city)" : city.trim();


            builder.append('\t').append(id).append(": ")
                    .append(fname).append(", ").append(lname)
                    .append(" - ").append(city).append('\n');
        }
    }

    public String showCustomer(int customerID) {
        StringBuilder builder = new StringBuilder();

        try {
            Statement stmt = conn.createStatement();

            ResultSet query = stmt.executeQuery("SELECT * FROM customer WHERE customerid = " + customerID);

            builder.append("Customer\n");

            if (query.isBeforeFirst()) {
                buildCustomer(query, builder);

                PreparedStatement borrowedStmt = conn.prepareStatement("SELECT title, isbn FROM customer NATURAL JOIN cust_book NATURAL JOIN book WHERE customerid = ?");
                borrowedStmt.setInt(1, customerID);
                ResultSet borrowed = borrowedStmt.executeQuery();

                if (borrowed.isBeforeFirst()) {

                    builder.append("\tBooks borrowed:\n");
                    while (borrowed.next()) {

                        builder.append("\t\t")
                                .append(borrowed.getInt("isbn"))
                                .append(" - ")
                                .append(borrowed.getString("title").trim())
                                .append('\n');

                    }

                } else {
                    builder.append("\t(no books borrowed)\n");
                }


            } else {
                builder.append("\tNo customer found with id ").append(customerID).append('\n');
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

            builder.append("Customers\n");

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
            // test if any result was found
            if (!customer.isBeforeFirst()) {
                throw new SQLException("customer with id " + customerID + " does not exist");
            }

            ResultSet book = stmt.executeQuery("SELECT numofcop, numleft FROM book WHERE isbn = " + isbn + " FOR UPDATE");
            int numberLeft;
            // test if any result was found
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
            try {
                conn.rollback();
            } catch (SQLException e1) {
                e1.printStackTrace();
            }
        } finally {
            try {
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
                throw new SQLException("Customer " + customerID + " does not have a copy of " + isbn + " to return.");
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
            try {
                conn.rollback();
            } catch (SQLException e1) {
                e1.printStackTrace();
            }
        } finally {
            try {
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

    // ensure that if any unhandled error occurs the connection will be terminated
    @Override
    protected void finalize() throws Throwable {
        if (conn != null && !conn.isClosed()) {
            conn.close();
        }
        super.finalize();
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
                    .append(")").append('\n');
        } catch (SQLException e) {
            builder.append("Error occurred while deleting customer ").append(customerID).append('\n');
            builder.append(e.getMessage()).append('\n');
            try {
                conn.rollback();
            } catch (SQLException e1) {
                e1.printStackTrace();
            }
        } finally {
            try {
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

            // Author -> Book_Author referential constraint to set to default on delete
            PreparedStatement bookAuthorDefault = conn.prepareStatement("UPDATE book_author SET authorid = 0 WHERE authorid = ?");
            bookAuthorDefault.setInt(1, authorID);
            bookAuthorDefault.executeUpdate();

            PreparedStatement customerDelete = conn.prepareStatement("DELETE FROM author WHERE authorid = ?");
            customerDelete.setInt(1, authorID);
            int borrowResult = customerDelete.executeUpdate();
            if (borrowResult == 0) {
                throw new SQLException("Failed to delete from author table");
            }

            conn.commit();
            builder.append("Successfully deleted author (").append(authorID)
                    .append(")").append('\n');
        } catch (SQLException e) {
            builder.append("Error occurred while deleting author ").append(authorID).append('\n');
            builder.append(e.getMessage()).append('\n');
            try {
                conn.rollback();
            } catch (SQLException e1) {
                e1.printStackTrace();
            }
        } finally {
            try {
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

            // Book -> Cust_Book referential requirement, restrict
            ResultSet book = stmt.executeQuery("SELECT 1 FROM cust_book WHERE isbn = " + isbn);
            if (book.isBeforeFirst()) {
                throw new SQLException("A copy of the book is borrowed, cannot deleted book until all copies are returned");
            }

            // Book -> Book_Author referential requirement, set to default
            PreparedStatement bookAuthorDefault = conn.prepareStatement("UPDATE book_author SET isbn = 0 WHERE isbn = ?");
            bookAuthorDefault.setInt(1, isbn);
            bookAuthorDefault.executeUpdate();

            PreparedStatement bookDelete = conn.prepareStatement("DELETE FROM book WHERE isbn = ?");
            bookDelete.setInt(1, isbn);
            int borrowResult = bookDelete.executeUpdate();
            if (borrowResult == 0) {
                throw new SQLException("Failed to delete from book table");
            }

            conn.commit();
            builder.append("Successfully deleted book (").append(isbn).append(")").append('\n');
        } catch (SQLException e) {
            builder.append("Error occurred while deleting book ").append(isbn).append('\n');
            builder.append(e.getMessage()).append('\n');
            try {
                conn.rollback();
            } catch (SQLException e1) {
                e1.printStackTrace();
            }
        } finally {
            try {
                conn.setAutoCommit(true);
            } catch (SQLException e) {
                e.printStackTrace();
            }
        }
        return builder.toString();
    }
}
