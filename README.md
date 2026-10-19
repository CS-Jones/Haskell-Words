# Haskell Text Analyzer & Histogram Generator 📊

A functional text-processing tool written in **Haskell** that reads a plain text file, normalizes the text data, filters out common English stop words, and generates a terminal-based visual ASCII histogram of the most frequent words.

## 🚀 Features

* **Text Normalization:** Strips out punctuation elements (`,.?!-:;"'`) and maps all characters uniformly to lowercase.
* **Stop Word Filtering:** Automatically detects and isolates 20 of the most common English words (such as *"the"*, *"and"*, *"of"*, etc.) from frequency counting.
* **Frequency Sorting:** Groups, aggregates, and sorts non-common words from highest to lowest occurrences.
* **ASCII Histogram Generation:** Renders an interactive, side-by-side terminal bar chart (`*** -> word`) matching word frequency metrics up to the top 20 entries.

## 🛠️ Tech Stack & Requirements

* **Language:** Haskell (GHC - Glasgow Haskell Compiler)
* **Standard Core Libraries:**
  * `Data.Char` - Used for converting text tokens to lowercase.
  * `Data.List` - Used for grouping, sorting, and intersection math.
  * `Data.Ord` - Used for data-sorting comparisons via `comparing`.

## 💻 Installation & Compilation

To compile and run this application locally, you will need the [Glasgow Haskell Compiler (GHC)](https://haskell.org) or the [Haskell Toolstack](https://haskellstack.org) installed.

1. **Clone the repository:**
   ```bash
   git clone https://github.com
   cd YOUR_REPO_NAME
   ```

2. **Compile using GHC:**
   ```bash
   ghc -o text_analyzer main.hs
   ```

## ⚙️ Usage

1. Make sure you have a target text file (e.g., `sample.txt`) ready in your directory.
2. Run the compiled executable:
   ```bash
   ./text_analyzer
   ```
3. When prompted by the terminal (`Introduce a filename:`), type the path to your file and press **Enter**:
   ```text
   Introduce a filename:
   sample.txt
   ```

### Example Terminal Output
```text
Report:
	142 words
	38 common words

Histogram of the most frequent words (excluding common words):
"*****"  ->  haskell
"***"    ->  functional
"**"     ->  programming
"*"      ->  data
```

## 📄 License

This project is open-source and available under the [MIT License](LICENSE).
