// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP112 Assignment 1
 * Name: David Barnett
 * Usercode: barnetdavi
 * ID: 300313764
 */

import ecs100.*;
import java.awt.Color;

/**
 * Checks dates, prints out in long format and draws calendars.
 * The processDates method
 * Reads a date from the user as three integers, and then
 * (a) checks that the date is valid (ie, represents a real date,
 *     taking into account leap years), reporting if it is not valid.
 *     (It may assume the standard modern calendar, and isn't required
 *      to give correct answers for dates before the 1600's when the
 *      calendar was different.
 * (b) If the date is valid, it prints out the date in a long form: eg
 *     Monday 3rd March, 2014. This requires working out which day
 *     of the week the date is.
 *     (Core only needs dates this year)
 * (c) It draws a one week calendar, highlighting the date:
 *     It shows the seven days of the week as a row of rectangles, highlighting
 *      the day corresponding to the date. It doesn't need to show the dates
 *      for each day.
 * (d) (Completion) It draws a monthly calendar for the month containing the date:
 *     It should draw a title containing the month and the year, and a
 *     grid of rectangles for each day of the month, giving the day of the month
 *     in each rectangle. This will be between 4 rows and 6 rows of 7 rectangles.
 *     The ISO standard for calendars specifies that the first day of each week
 *     should be a Monday.
 *     Ideally, the calendar should include the last few days of the previous
 *     month when the month doesn't start on a Monday and the first few days
 *     of the next month when the month doesn't end on a Sunday.
 *
 * Reasonable design would have a number of methods, for example:
 *  isValidDate  which would return a boolean (true or false)
 *  isLeapYear   which would return a boolean (true or false)
 *  findDay      which would return the day of the week as an int (0 to 6)
 *  drawWeek     which would draw the weekly "calendar"
 *  drawMonth    which would draw the monthly calendar
 * You might choose to design it differently, but doing it all in one huge method
 *  would not be good design.
*/

public class DateChecker {

    // constants:
    /*# YOUR CODE HERE */

    /**
     * Loop to repeatedly process a date.
     * Asks the user if they want to enter another date each time.
     */
    public void processDates(){
        do  {
            UI.clearText();
            UI.clearGraphics();
            processADate();
        }
        while (UI.askBoolean("Enter another date?")); 
        UI.quit();
    }

    /**
     * Asks user for a date, then produces the required output
     */
    public void processADate(){
        int day   = UI.askInt("Enter the Day : ");
        int month = UI.askInt("Enter the Month : ");
        int year  = UI.askInt("Enter the Year : ");
        
        if (isValidDate(day , month , year))
        {
            UI.println( weekDayToString(findDay(day,month,year)) + " " + day + getDateSuffix(day) + " " + monthToString(month) + ", " + year );
            drawMonth(day,month,year);
        }
    }
    
    private boolean isLeapYear (int year)
    {
        if (year % 400 == 0)
        {
            return true;
        } else if (year % 100 == 0)
        {
            return false;
        } else if (year % 4 == 0)
        {
            return true;
        }
        return false; 
    }
    
    private boolean isValidDate(int day, int month, int year)
    {
        //Check if Months are between 1 and 12
        if (month < 1 || month > 12)
        {
            return false;
        }
        
        if (day < 1 || day > lengthOfMonth(month , year))
        {
            return false;
        }
        
        return true;
    }
    
    private int lengthOfMonth (int month , int year)
    {
        //Figure out the number of days in month
        //Using Method described in https://en.wikipedia.org/wiki/Gregorian_calendar#Description
        int daysInMonth = 30 + ( ( month +  (int)(java.lang.Math.floor(month/8)) ) % 2 );
        //Febuary gets special treatment
        if (month == 2)
        {
            // In leap years only minus 1, other than that minus 2
            if (isLeapYear(year)) {
                daysInMonth -= 1;
            } else {
                daysInMonth -= 2;                
            }
        }
        
        return daysInMonth;
    }
    
    private int findDay(int day, int month , int year)
    {   
        //Method of Calculating day of the week below
        //https://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week#A_tabular_method_to_calculate_the_day_of_the_week
        //Convert Month's into Months Table equalivant
        switch (month)
        {
            case(1):
                if (isLeapYear(year))
                {
                    month = -1;
                } else {
                    month  = 0;
                }
                break;
            case(2):
                if (isLeapYear(year))
                {
                    month = 2;
                } else {
                    month  = 3;
                }
                break;
            case(3):
            case(11):
                month = 3;
                break;
           case (4):
           case (7):
                month = 6;
                break;
           case (5):
                month = 1;
                break;
           case (6):
                month = 4;
                break;
           case (8):
                month = 2;
                break;
           case (9):
           case (12):
                month = 5;
                break;
           case (10):
                month = 0;
                break; 
        }
        
        int c = year / 100;
        
        c = 6 - 2 * (c % 4);
        
        
        //Get the last two digits of the year
        year = year % 100;
        
        //Calculate weekday
        // 0 => Sunday
        // 1 => Monday...
        int weekday = (day + month + year + (year/4) + c ) % 7;
        
        return weekday;
    }
    
    private String weekDayToString(int weekday)
    {
        switch (weekday)
        {
            case 1: return "Monday";
            case 2: return "Tuesday";
            case 3: return "Wednesday";
            case 4: return "Thrusday";
            case 5: return "Friday";
            case 6: return "Saturday";
            case 0: return "Sunday";
            default:
                break;
        }
        return "Invalid Date";
    }
    
    private String monthToString(int month)
    {
        switch (month)
        {
            case 1:  return "January";
            case 2:  return "February";  
            case 3:  return "March";
            case 4:  return "April";
            case 5:  return "May";
            case 6:  return "June";
            case 7:  return "July";
            case 8:  return "August";
            case 9:  return "September";
            case 10: return "October";
            case 11: return "November";
            case 12: return "December";
            default: return "Invalid month";
                     
        }
    }
    
    private String getDateSuffix (int day)
    {
        switch (day)
        {
            case 1 : return "st";
            case 2 : return "nd";
            case 3 : return "rd";
            default : return "th";
        }
    }
    
    public void drawMonth (int day , int month , int year)
    {
        //Draw Header with month and year
        UI.clearGraphics(false);
        UI.setColor(Color.black);
        UI.setFontSize(36);
        UI.drawString(monthToString(month) + " " + year ,0, 36);
        
        int width = 64;
        int height = 48;
        
        int weekday = findDay(day,month,year);
        
        
        //Draw Week days
        for (int i = 0; i  < 7; i++)
        {
            if (weekday == i+1 || (i == 6 && weekday == 0))
            {
                UI.setColor(Color.green);
                UI.fillRect(0 + i*width, 48, width, 24);
            }
            UI.setColor(Color.black);
            UI.drawRect(0 + i*width, 48, width, 24);
            
            UI.setFontSize(11);
            if (i != 6)
            {
                UI.drawString( weekDayToString(i+1) , (i*width) + 5 , height+11);
            } else {
                UI.drawString( weekDayToString(0) , (i*width) + 5 , height+11);
            }
        }
        
        //Draw dates
        
        //Find the year and month of the last month
        int lastMonth_month = month - 1;
        int lastMonth_year = year;
        if (month < 1)
        {
            month = 12;
            lastMonth_year--;
        }
        //Find how many days of last month is needed
        int numOfLastMonth = findDay(1,month,year);
        switch (numOfLastMonth)
        {
            //Sunday
            case(0):
                numOfLastMonth = 6;
                break;
            //All other days
            default:
                numOfLastMonth--;
                break;
        }
        
        //Find the year and month of the next month
        int nextMonth_month = month + 1;
        int nextMonth_year = year;
        if (month > 12)
        {
            month = 1;
            lastMonth_year++;
        }
        //Find how many days of next month is needed
        int numOfNextMonth = findDay( lengthOfMonth(month,year) ,month,year);
        switch (numOfLastMonth)
        {
            //Sunday
            case(0):
                numOfNextMonth = 0;
                break;
            //All other days
            default:
                numOfNextMonth--;
                numOfNextMonth = 6 - numOfNextMonth;
                break;
        }
        
        //Start date
        int Date = lengthOfMonth(lastMonth_month, lastMonth_year) - numOfLastMonth +1 ;
        boolean thisMonth = false;
        int base_x = 0;
        int base_y = 80;
        for (int rows = 0; rows < 6; rows++)
        {
            int y = base_y + rows*height;
            for (int cell = 0; cell < 7; cell++)
            {
                int x = base_x + cell*width;
                if (thisMonth && Date == day)
                {
                    UI.setColor(Color.green);
                    UI.fillRect(x, y, width, height);
                }
                UI.setColor(Color.black);
                UI.drawRect(x, y, width, height);
                
                if (thisMonth){
                    UI.setFontSize(12);
                    UI.setColor(Color.black);
                }else{
                    UI.setFontSize(11);
                    UI.setColor(Color.gray);
                }
                
                UI.drawString("" + Date, x + 5 , y +12);
                
                Date++;
                if (Date > lengthOfMonth(lastMonth_month, lastMonth_year) && thisMonth == false)
                {
                    Date = 1;
                    thisMonth = true;
                }
                if (Date > lengthOfMonth(month, year) && thisMonth == true)
                {
                    Date = 1;
                    thisMonth = false;
                }
            }
        }
        
        //Show the Calender
        UI.repaintGraphics();
    }
    
    // Main
    /** Create a new DateChecker object and call processDates */
    public static void main(String[] arguments){
        UI.initialise();
        DateChecker dc = new DateChecker();
        dc.processDates();
    }        

}
