import java.io.*;
import java.util.HashMap;
import java.util.Scanner;

import ecs100.*; 

public class WordCloudUI {
	
	private HashMap<String,Integer> words;
	
	public WordCloudUI()
	{
		words = new HashMap<String,Integer>();
		init();
	}
	
	private void init()
	{
		UI.initialise();
		UI.setImmediateRepaint(false);
		
		UI.addButton("Load", new UIButtonListener() {
			
			@Override
			public void buttonPerformed(String name) {
				load(UIFileChooser.open());
			}
		});
	}
	
	public void load(String path)
	{
		try {
			BufferedReader file = new BufferedReader(new FileReader(path));
			String line = "";
			while ( (line = file.readLine()) != null)
			{
				for (String word : line.split(" "))
				{
					word = word.trim();
					if (word.isEmpty())
						continue;
					
					words.put(word, (words.containsKey(word) ? words.get(word) + 1 : 1 ));
				}
			}
			file.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		for (String key : words.keySet())
		{
			UI.printf("%s:%d\n", key , words.get(key));
		}
	}
	
	public static void main(String[] args) {
		new WordCloudUI();
	}
}