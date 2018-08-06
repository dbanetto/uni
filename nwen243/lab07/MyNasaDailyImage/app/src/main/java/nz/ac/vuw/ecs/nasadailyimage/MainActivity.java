package nz.ac.vuw.ecs.nasadailyimage;

import android.app.Activity;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;

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
    private Bitmap bitmap = null;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        new MainTask().execute();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_main, menu);
        return true;
    }


    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_settings) {
            return true;
        }

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

        if (image == null) {
            return;
        }

        //##Missing##
        //Update the text content of the TextView widget "imageTitle"
        TextView imageTitle = (TextView) findViewById(R.id.imageTitle);
        imageTitle.setText(image.getTitle());

        //##Missing##
        //Update the text content of the TextView widget "imageDate"
        TextView imageDate = (TextView) findViewById(R.id.imageDate);
        imageDate.setText(image.getDate());

        //##Missing##
        //Update the text content of the TextView widget "imageDescription"
        TextView imageDescription = (TextView) findViewById(R.id.imageDescription);
        imageDescription.setText(image.getDescription());

        //##Missing## Here we missed a WebView widget.
        //Update the content of the WebView widget "imageView"
        //Please create a WebView widget on the main layout (i.e., activity_main.xml) to
        //connect with the following commented codes.


        ImageView imageView = (ImageView) findViewById(R.id.imageView);

        //Resize the content of the WebView widget
        imageView.setAdjustViewBounds(true);

        //Display the image by its url
        if (bitmap != null) {
            imageView.setVisibility(View.VISIBLE);
            imageView.setImageBitmap(bitmap);
            imageView.setMaxHeight(bitmap.getHeight());
            imageView.setMaxWidth(bitmap.getWidth());
        } else {
            imageView.setVisibility(View.INVISIBLE);
        }

    }

    private Bitmap getBitmap(String urlImage) {

        BitmapFactory.Options bmOptions;
        bmOptions = new BitmapFactory.Options();
        bmOptions.inSampleSize = 1;

        Bitmap bitmap = null;
        InputStream in;
        try {
            URL url = new URL(urlImage);
            Log.d("Url", urlImage);
            URLConnection conn = url.openConnection();
            Log.d("Url", "Connected");
            try{
                HttpURLConnection httpConn = (HttpURLConnection)conn;
                httpConn.setRequestMethod("GET");
                httpConn.connect();
                Log.d("Url", "HTTP Connected " + httpConn.getResponseCode());
                if (httpConn.getResponseCode() == HttpURLConnection.HTTP_OK) {
                    in = httpConn.getInputStream();
                    Log.d("Url", "Got input stream");
                    bitmap = BitmapFactory.decodeStream(in, null, bmOptions);
                    Log.d("Url", "Got bitmap");
                    in.close();
                }
            }
            catch (Exception ex)
            {
                throw new RuntimeException(ex);
            }
        } catch (IOException ei) {
            throw new RuntimeException(ei);
        }

        return bitmap;
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
            processFeed();
            return null;
        }

        @Override
        protected void onPostExecute(Void aVoid) {

            super.onPostExecute(aVoid);

            //##Missing##
            //Invoke the function to reset display after the latest daily image obtained.
            if (image != null) {
                resetDisplay();
            }
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
                bitmap = getBitmap(image.getUrl());
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }
}
