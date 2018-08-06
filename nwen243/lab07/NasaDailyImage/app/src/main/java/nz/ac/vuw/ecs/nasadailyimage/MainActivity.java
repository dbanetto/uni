package nz.ac.vuw.ecs.nasadailyimage;

import android.app.Activity;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.webkit.WebView;
import android.widget.TextView;

import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;

import java.io.InputStream;
import java.net.URL;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;


/**
 * <h1>Nasa Daily Image Main Activity</h1>
 * The program implements an application that
 * simply acquires the latest daily image from NASA RSS and displays the image
 * with its relative information (e.g, title, description and date) on the screen.
 * <p/>
 * <b>Note:</b>
 * 1. The program currently doesn't run correctly due to some codes are missing, please complete the
 *    program in the lab session.
 * 2. All the missing parts are marked by the label "##Missing##", please search in the entire
 *    project by using the keyword to assure completeness.
 * 3. Please demo your work to your lab tutor by running the application successfully.
 *
 * @author Aaron Chen
 * @version 1.0
 * @since 2015-08-31
 */
public class MainActivity extends Activity {

    private static final String URL = "http://www.nasa.gov/rss/dyn/image_of_the_day.rss";

    private Image image = null;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        //setContentView(R.layout.activity_main);
        new MainTask().execute();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        //getMenuInflater().inflate(R.menu.menu_main, menu);

        return true;
    }


    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        //if (id == R.id.action_settings) {
        //    return true;
        //}

        return super.onOptionsItemSelected(item);
    }

    /**
     * This method is used to reset the display on screen after
     * retrieving the image from RSS.
     *
     * @param
     * @return Nothing.
     */
    public void resetDisplay() {

        //##Missing##
        //Update the text content of the TextView widget "imageTitle"


        //##Missing##
        //Update the text content of the TextView widget "imageDate"


        //##Missing##
        //Update the text content of the TextView widget "imageDescription"


        //##Missing## Here we missed a WebView widget.
        //Update the content of the WebView widget "imageView"
        //Please create a WebView widget on the main layout (i.e., activity_main.xml) to
        //connect with the following commented codes.

        /*
        WebView imageView = (WebView) findViewById(R.id.imageView);

        //Resize the content of the WebView widget
        imageView.setInitialScale(1);
        imageView.getSettings().setJavaScriptEnabled(true);
        imageView.getSettings().setLoadWithOverviewMode(true);
        imageView.getSettings().setUseWideViewPort(true);
        imageView.setScrollBarStyle(WebView.SCROLLBARS_OUTSIDE_OVERLAY);
        imageView.setScrollbarFadingEnabled(false);

        //Display the image by its url
        imageView.loadUrl(image.getUrl());
        */
    }

    /**
     * This inner class inherits from AsyncTask which performs background
     * operations and publish results on the UI thread.
     */
    public class MainTask extends AsyncTask<Void, Void, Void> {

        @Override
        protected void onProgressUpdate(Void... values) {
            super.onProgressUpdate(values);
        }

        @Override
        protected Void doInBackground(Void... params) {
            //##Missing##
            //Invoke the function to retrieve the image from NASA RSS feed.

            return null;
        }

        @Override
        protected void onPostExecute(Void aVoid) {

            super.onPostExecute(aVoid);

            //##Missing##
            //Invoke the function to reset display after the latest daily image obtained.
        }

        /**
         * This method is used to retrieve the latest daily image from NASA RSS feed.
         * @param
         * @return Nothing.
         */
        public void processFeed() {
            try {
                SAXParserFactory saxParserFactory =
                        SAXParserFactory.newInstance();
                SAXParser parser = saxParserFactory.newSAXParser();
                XMLReader reader = parser.getXMLReader();
                IotdHandler iotdHandler = new IotdHandler();
                reader.setContentHandler(iotdHandler);

                InputStream inputStream = new URL(URL).openStream();
                reader.parse(new InputSource(inputStream));

                image = iotdHandler.getImage();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }
}
