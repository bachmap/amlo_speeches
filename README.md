README: AMLO Media Attacks Analysis

This project analyzes President Andrés Manuel López Obrador’s (AMLO) verbal attacks on media and journalists during his morning press conferences ("mañaneras"), using YouTube transcripts, sentiment lexicons, and approval rating data.

--------------------------------------------
1_get_yt_amlo_metadata.ipynb
--------------------------------------------
Purpose:
Scrapes metadata from AMLO’s official YouTube videos (title, date, view count, like count, etc.).

Output:
CSV file with video-level metadata, including video_id and upload_date.

--------------------------------------------
2_Amlo_scrape.ipynb
--------------------------------------------
Purpose:
Scrapes and cleans the transcript texts of AMLO’s mañaneras using YouTube subtitles.

Output:
CSV file with full transcripts, one row per video.

--------------------------------------------
3_get_media_journalist_corpus.R
--------------------------------------------
Purpose:
Generates a dictionary of Mexican media outlets and journalist names.

Steps:
- Scrapes names from Wikipedia categories.
- Cleans and filters entries.
- Adds a manual list of important journalists/media.

Output:
media_targets.txt — a cleaned list of names for identifying mentions in transcripts.

--------------------------------------------
4_analysis.R
--------------------------------------------
Purpose:
Main analysis script that:
- Detects media mentions and classifies sentiment (positive, negative, neutral).
- Aggregates verbal media attacks over time (monthly/quarterly).
- Merges with political event and approval rating data.
- Fits ARIMA and count models to explain attack frequency.
- Analyzes impact of attacks on video views and likes.
- Generates summary plots and word clouds.
