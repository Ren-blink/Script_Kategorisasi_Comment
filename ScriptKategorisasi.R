library(dplyr)
library(stringr)

df <- cleaned_tiktok_comments1 #Jangan lupa ganti nama filenya

#declare pattern when_yah dan my_claim dengan kata-kata utk membantu kategorisasi

pattern_when_yah <- paste(
  "\\b(when+|wen+)\\s*(ya+h*|yh)\\b",
  "\\b(kapan+|kpn|kpan)\\s*(ya+h*|yh|ye+h*)\\b",
  "\\b(kapan+|when+)\\s*ya\\s*(allah|alloh)\\b",
  "\\b(when+|wen+|kapan+|kpn|kpan)\\b",
  "\\b(kpn|kpan)\\s*(sih|si|nih|dong|deh)\\b",
  "\\bwen\\s*(sih|si)\\b",
  "\\bwhn\\s*(yh|ya+h*)\\b",
  "\\b(kpn|kapan)\\s*cuy\\b", 
  "\\bkpn\\s*yak?\\b",
  "\\b(when|wen|kapan|kpn)\\b",
  sep = "|"
)

pattern_my_claim <- paste(
  "\\bmy\\s+\\w+",
  "\\b\\s+my\\b",
  "\\b\\s+gua\\b",
  "\\bmy\\s+kisah\\b",
  "\\bkisah\\s+gw\\b",
  "\\bkisah\\s*gw\\b",
  "\\bkisah\\s+gua\\b",
  "\\bkisah\\s*gua\\b",
  "\\bmy\\s+story\\b",
  "\\bmy\\b",
  "\\bbukankah\\s+ini\\s+my\\b",
  "\\bapakah\\b",
  "\\bapa\\b",
  "\\bini\\s*kan\\b",
  "\\bini(\\s+mah)?\\s+(punyaku|milikku|pnyaku|mlkku)\\b",
  "\\bini(\\s+(kan|lah|mah|dong|sih|nih))?\\s+my\\b",
  "\\blah\\s+ini\\b",
  "\\b(itu|ini)(\\s+(mah|kan|lah|dong|sih|nih))?\\s+my\\b",
  "\\b(punyaku|milikku|pnyaku|mlkku|pnyq|pny)\\b",
  "\\bini\\s+gw\\s+pnya\\b",              
  "\\bini\\s+(gue|gw|ane)\\s+(punya|pny)\\b", 
  "\\b(gw|gue|ane)\\s+(pnya|pny)\\b",     
  "\\bini\\s+tuh\\s+my\\b",               
  "\\bmy\\s+(bgt|banget|bngett?)\\b",     
  "\\bkan\\s+(punyaku|pnyaku|pnyq)\\b",   
  "\\bmy\\b",                             
  sep = "|"
)

#fungsi categorize yang nerima parameter comment dan mengasumsikan kalau case insensitive secara default
categorize_comment <- function(comment, case_sensitive = FALSE) {

#Cek apabila comment kosong atau tidak
  if (is.na(comment) || trimws(as.character(comment) == "")) {
    return("Tidak Terkategorikan")
  }

#Konversi ke character
comment_text <- as.character(comment)

#Mengubah text comment menjadi lowercase (apabila tidak) agar case-insensitive  
  if (!case_sensitive){
  comment_lower <- tolower(comment_text)
  }
#Deteksi pattern when_yah dan my_claim dengan str_detect
has_when_yah <- str_detect(comment_lower, pattern_when_yah)
has_my_claim <- str_detect(comment_lower, pattern_my_claim)

#Mengkategorikan buat masuk ke FR yang mana (atau masuk ke TT)
if (has_when_yah && has_my_claim) {
  return("FR-03")
} else if (has_when_yah) {
  return("FR-01")
} else if (has_my_claim) {
  return("FR-02")
} else {
  return("Tidak Terkategorikan")
}

}

#Menerapkan fungsi categorize_comment ke kolom Comments di dataframe df
df$Kategori <- sapply(df$Comments, categorize_comment)

#Menambahkan kolom Deskripsi_Kategori berdasarkan nilai di kolom Kategori
df$Deskripsi_Kategori <- case_when(
  df$Kategori == "FR-01" ~ "When Yah",
  df$Kategori == "FR-02" ~ "My Claim",
  df$Kategori == "FR-03" ~ "When Yah & My Claim",
  TRUE ~ "Tidak Terkategorikan"
)
#Ekspor dataframe df ke file .csv
write.csv(df, "Data_Comment_Kategorisasi_VT5.csv", row.names = FALSE) #Ganti nama file yang mau diimport

#cek di dir mana
getwd()

#preview filenya
head(df)