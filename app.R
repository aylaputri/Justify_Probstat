library(shiny)
library(bslib)

# ---- Data contoh (Bike Sharing UCI) ----
contoh <- read.csv("data/day.csv")
contoh$season  <- NULL   # tidak dipakai (konteks Indonesia)
contoh$instant <- NULL   # hanya nomor urut
contoh$hari   <- factor(contoh$workingday, levels = c(1, 0),
                        labels = c("Hari kerja", "Non-hari kerja"))
contoh$cuaca  <- factor(contoh$weathersit, levels = 1:3,
                        labels = c("Cerah", "Berkabut/Berawan", "Hujan/Salju ringan"))
contoh$suhu_c <- contoh$temp * 41   # kembalikan ke Celsius

# format p-value
fmt_p <- function(p) ifelse(p < .001, "< 0.001", sprintf("%.3f", p))

# ---- Tampilan ----
ui <- page_navbar(
  title = "Statistika Interaktif",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,
  sidebar = sidebar(
    title = "Data",
    radioButtons("sumber", "Sumber data",
                 c("Data contoh (Bike Sharing)" = "contoh",
                   "Unggah CSV saya" = "unggah")),
    conditionalPanel("input.sumber == 'unggah'",
                     fileInput("berkas", "Pilih file CSV", accept = ".csv")),
    hr(),
    selectInput("var_y", "Variabel numerik (dianalisis)", choices = NULL),
    selectInput("var_g", "Variabel kelompok (2 kategori)", choices = NULL)
  ),
  
  # ---------- Halaman 1 ----------
  nav_panel(
    "Ringkasan Dataset",
    uiOutput("kotak"),
    card(card_header("Cuplikan data"),
         div(style = "overflow-x:auto;", tableOutput("tabel_awal"))),
    card(card_header("Pemeriksaan variabel"), tableOutput("tabel_variabel"))
  ),
  
  # ---------- Halaman 2 ----------
  nav_panel(
    "Statistik Deskriptif",
    layout_columns(
      fill = FALSE,
      card(card_header("Seluruh data"), tableOutput("tabel_desk")),
      card(card_header("Per kelompok"), tableOutput("tabel_desk_grup"))
    ),
    layout_columns(
      fill = FALSE,
      card(card_header("Histogram"), plotOutput("hist_y")),
      card(card_header("Boxplot per kelompok"), plotOutput("box_y"))
    ),
    card(card_header("Interpretasi"), uiOutput("interpretasi_desk"))
  ),
  
  # ---------- Halaman 3 ----------
  nav_panel(
    "Probabilitas & Distribusi",
    layout_columns(
      fill = FALSE,
      card(card_header("Histogram + kurva normal"), plotOutput("hist_norm")),
      card(card_header("Q-Q plot normal"), plotOutput("qq_y"))
    ),
    card(card_header("Ukuran bentuk sebaran"), tableOutput("tabel_normal")),
    card(
      card_header("Hitung peluang"),
      layout_columns(
        fill = FALSE,
        numericInput("ambang", "Ambang (x)", value = 0),
        selectInput("arah", "Peluang", c("lebih dari ambang" = "lebih",
                                         "kurang dari ambang" = "kurang"))
      ),
      tableOutput("tabel_peluang"),
      uiOutput("interpretasi_peluang")
    ),
    card(
      card_header("Peluang hari ramai menurut kategori"),
      p("Hari ramai = nilai variabel numerik lebih dari ambang di atas."),
      selectInput("var_k", "Kategori pembanding", choices = NULL),
      tableOutput("tabel_bersyarat"),
      uiOutput("interpretasi_bersyarat")
    )
  ),
  
  # ---------- Halaman 4 ----------
  nav_panel(
    "Estimasi Parameter",
    card(
      card_header("Interval kepercayaan rata-rata"),
      selectInput("cl", "Tingkat kepercayaan",
                  c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99), selected = 0.95),
      tableOutput("tabel_ci"),
      uiOutput("interpretasi_ci"),
      p(em("Catatan: interval ini mengasumsikan pengamatan saling bebas. Data harian berurutan biasanya saling berkaitan, sehingga interval cenderung lebih sempit dari yang seharusnya."))
    )
  ),
  
  # ---------- Halaman 5 ----------
  nav_panel(
    "Uji Hipotesis",
    card(
      card_header("Rumusan hipotesis"),
      uiOutput("rumusan_h"),
      selectInput("alpha", "Taraf signifikansi (\u03b1)",
                  c("0.01", "0.05", "0.10"), selected = "0.05")
    ),
    card(
      card_header("Langkah 1: periksa asumsi"),
      tableOutput("tabel_asumsi"),
      tableOutput("tabel_varians"),
      plotOutput("qq_grup", height = "300px"),
      uiOutput("kesimpulan_asumsi")
    ),
    card(
      card_header("Langkah 2: uji dan keputusan"),
      p("Uji utama: Welch t-test, karena tidak mengasumsikan varians kedua kelompok sama dan aman untuk kelompok berukuran berbeda. Mann-Whitney U dipakai sebagai pembanding."),
      tableOutput("tabel_uji"),
      uiOutput("keputusan_uji")
    )
  )
)

# ---- Logika ----
server <- function(input, output, session) {
  
  data_aktif <- reactive({
    if (input$sumber == "contoh") return(contoh)
    validate(need(input$berkas, "Silakan unggah file CSV di panel kiri."))
    d <- tryCatch(read.csv(input$berkas$datapath), error = function(e) NULL)
    if (!is.null(d) && ncol(d) == 1) d <- read.csv2(input$berkas$datapath)  # CSV titik koma
    validate(need(!is.null(d) && nrow(d) > 1, "File tidak bisa dibaca sebagai CSV."))
    d
  })
  
  # isi pilihan variabel setiap kali data berganti
  observeEvent(data_aktif(), {
    d   <- data_aktif()
    num <- names(d)[sapply(d, is.numeric)]
    if (input$sumber == "contoh") {
      num <- intersect(c("cnt", "casual", "registered", "suhu_c", "hum", "windspeed"), num)
    }
    kel <- names(d)[sapply(d, function(k) length(unique(na.omit(k))) == 2)]
    kat <- names(d)[sapply(d, function(k) length(unique(na.omit(k))) %in% 2:6)]
    updateSelectInput(session, "var_y", choices = num,
                      selected = if ("cnt" %in% num) "cnt" else head(num, 1))
    updateSelectInput(session, "var_g", choices = kel,
                      selected = if ("hari" %in% kel) "hari" else head(kel, 1))
    updateSelectInput(session, "var_k", choices = kat,
                      selected = if ("cuaca" %in% kat) "cuaca" else head(kat, 1))
  })
  
  # ===== Halaman 1 =====
  output$kotak <- renderUI({
    d <- data_aktif(); req(input$var_y)
    layout_columns(
      fill = FALSE,
      value_box("Jumlah baris", nrow(d)),
      value_box("Jumlah kolom", ncol(d)),
      value_box(paste("Rata-rata", input$var_y),
                round(mean(d[[input$var_y]], na.rm = TRUE), 2))
    )
  })
  
  output$tabel_awal <- renderTable(head(data_aktif(), 10))
  
  output$tabel_variabel <- renderTable({
    d <- data_aktif()
    data.frame(
      Variabel   = names(d),
      Tipe_di_R  = sapply(d, function(k) class(k)[1]),
      Nilai_unik = sapply(d, function(k) length(unique(k))),
      Kosong     = sapply(d, function(k) sum(is.na(k))),
      row.names  = NULL
    )
  })
  
  # ===== Halaman 2 =====
  ringkas <- function(x) {
    x <- x[!is.na(x)]
    data.frame(N = length(x), Mean = mean(x), Median = median(x),
               Varians = var(x), SD = sd(x), Min = min(x), Maks = max(x))
  }
  
  output$tabel_desk <- renderTable({
    d <- data_aktif(); req(input$var_y)
    ringkas(d[[input$var_y]])
  }, digits = 2)
  
  output$tabel_desk_grup <- renderTable({
    d <- data_aktif(); req(input$var_y, input$var_g)
    g <- factor(d[[input$var_g]])
    do.call(rbind, lapply(levels(g), function(l)
      cbind(Kelompok = l, ringkas(d[[input$var_y]][which(g == l)]))))
  }, digits = 2)
  
  output$hist_y <- renderPlot({
    d <- data_aktif(); req(input$var_y)
    hist(d[[input$var_y]], col = "#2C7BE5", border = "white",
         main = NULL, xlab = input$var_y, ylab = "Frekuensi")
  })
  
  output$box_y <- renderPlot({
    d <- data_aktif(); req(input$var_y, input$var_g)
    boxplot(d[[input$var_y]] ~ factor(d[[input$var_g]]),
            col = "#9CC3F5", xlab = input$var_g, ylab = input$var_y)
  })
  
  output$interpretasi_desk <- renderUI({
    d <- data_aktif(); req(input$var_y)
    x <- na.omit(d[[input$var_y]])
    m <- mean(x); md <- median(x); s <- sd(x)
    bentuk <- if (abs(m - md) / s < 0.1) {
      "kira-kira simetris"
    } else if (m > md) {
      "miring ke kanan (nilai besar menarik rata-rata ke atas)"
    } else {
      "miring ke kiri (nilai kecil menarik rata-rata ke bawah)"
    }
    p(sprintf("Rata-rata %s adalah %.2f dan mediannya %.2f, sehingga sebaran data %s. Simpangan baku %.2f berarti nilai umumnya menyimpang sekitar %.2f dari rata-rata.",
              input$var_y, m, md, bentuk, s, s))
  })
  
  # ===== Halaman 3 =====
  # ambang awal = kuartil 3 dari variabel terpilih
  observeEvent(input$var_y, {
    req(input$var_y)
    x <- na.omit(data_aktif()[[input$var_y]])
    updateNumericInput(session, "ambang", value = round(quantile(x, .75)))
  })
  
  output$hist_norm <- renderPlot({
    d <- data_aktif(); req(input$var_y)
    x <- na.omit(d[[input$var_y]]); m <- mean(x); s <- sd(x)
    hist(x, freq = FALSE, col = "#9CC3F5", border = "white",
         main = NULL, xlab = input$var_y, ylab = "Kepadatan")
    curve(dnorm(x, m, s), add = TRUE, lwd = 2, col = "#E55353")
  })
  
  output$qq_y <- renderPlot({
    d <- data_aktif(); req(input$var_y)
    x <- na.omit(d[[input$var_y]])
    qqnorm(x, main = NULL); qqline(x, col = "#E55353", lwd = 2)
  })
  
  output$tabel_normal <- renderTable({
    d <- data_aktif(); req(input$var_y)
    x <- na.omit(d[[input$var_y]])
    z <- (x - mean(x)) / sqrt(mean((x - mean(x))^2))
    pv <- if (length(x) >= 3 && length(x) <= 5000) shapiro.test(x)$p.value else NA
    data.frame(
      Ukuran = c("Skewness (kemiringan)", "Kurtosis kelebihan (0 = normal)", "Shapiro-Wilk p-value"),
      Nilai  = c(sprintf("%.2f", mean(z^3)), sprintf("%.2f", mean(z^4) - 3),
                 ifelse(is.na(pv), "-", fmt_p(pv)))
    )
  })
  
  hitung_peluang <- reactive({
    d <- data_aktif(); req(input$var_y, input$ambang, input$arah)
    x <- na.omit(d[[input$var_y]]); a <- input$ambang
    if (input$arah == "lebih") {
      c(mean(x > a), 1 - pnorm(a, mean(x), sd(x)))
    } else {
      c(mean(x < a), pnorm(a, mean(x), sd(x)))
    }
  })
  
  output$tabel_peluang <- renderTable({
    pl <- hitung_peluang()
    data.frame(Cara = c("Empiris (proporsi data)", "Model normal"),
               Peluang = sprintf("%.1f%%", pl * 100))
  })
  
  output$interpretasi_peluang <- renderUI({
    pl <- hitung_peluang()
    p(sprintf("Berdasarkan data, peluang %s %s adalah %.1f%%, sedangkan model normal memberi %.1f%% (selisih %.1f poin persen). Semakin kecil selisihnya, semakin cocok model normal untuk data ini.",
              input$var_y,
              ifelse(input$arah == "lebih", paste("lebih dari", input$ambang),
                     paste("kurang dari", input$ambang)),
              pl[1] * 100, pl[2] * 100, abs(pl[1] - pl[2]) * 100))
  })
  
  # peluang bersyarat: P(ramai | kategori)
  hitung_bersyarat <- reactive({
    d <- data_aktif(); req(input$var_y, input$var_k, input$ambang)
    x <- d[[input$var_y]]; k <- factor(d[[input$var_k]])
    ok <- !is.na(x) & !is.na(k)
    ramai <- x[ok] > input$ambang
    k <- droplevels(k[ok])
    tab <- data.frame(Kategori  = levels(k),
                      n         = as.integer(table(k)),
                      Rata_rata = as.numeric(tapply(x[ok], k, mean)),
                      Ramai     = as.integer(tapply(ramai, k, sum)))
    tab$Peluang <- tab$Ramai / tab$n
    list(tab = tab, total = mean(ramai), n = sum(ok), ramai = sum(ramai))
  })
  
  output$tabel_bersyarat <- renderTable({
    h <- hitung_bersyarat(); tb <- h$tab
    tb$Rata_rata <- sprintf("%.1f", tb$Rata_rata)
    tb$Peluang   <- sprintf("%.1f%%", tb$Peluang * 100)
    rbind(tb, data.frame(Kategori = "Semua data", n = h$n, Rata_rata = "-",
                         Ramai = h$ramai, Peluang = sprintf("%.1f%%", h$total * 100)))
  })
  
  output$interpretasi_bersyarat <- renderUI({
    tb <- hitung_bersyarat()$tab
    atas <- tb[which.max(tb$Peluang), ]; bawah <- tb[which.min(tb$Peluang), ]
    p(sprintf("Peluang hari ramai (%s > %s) paling tinggi pada kategori '%s' (%.1f%%) dan paling rendah pada '%s' (%.1f%%). Ini peluang bersyarat: P(ramai | kategori) = jumlah hari ramai di kategori itu dibagi jumlah hari di kategori itu. Kategori dengan n kecil menghasilkan angka yang kurang stabil.",
              input$var_y, input$ambang, atas$Kategori, atas$Peluang * 100,
              bawah$Kategori, bawah$Peluang * 100))
  })
  
  # ===== Halaman 4 =====
  hitung_ci <- reactive({
    d <- data_aktif(); req(input$var_y, input$cl)
    x <- na.omit(d[[input$var_y]]); n <- length(x)
    cl <- as.numeric(input$cl)
    m <- mean(x); se <- sd(x) / sqrt(n)
    tkrit <- qt(1 - (1 - cl) / 2, df = n - 1)
    list(n = n, cl = cl, m = m, se = se, t = tkrit,
         bawah = m - tkrit * se, atas = m + tkrit * se)
  })
  
  output$tabel_ci <- renderTable({
    h <- hitung_ci()
    data.frame(
      Ukuran = c("n", "Estimasi titik (rata-rata)", "Galat baku (SE)",
                 "Nilai t kritis", "Batas bawah", "Batas atas"),
      Nilai  = c(h$n, sprintf("%.2f", c(h$m, h$se, h$t, h$bawah, h$atas)))
    )
  })
  
  output$interpretasi_ci <- renderUI({
    h <- hitung_ci()
    p(sprintf("Estimasi rata-rata %s adalah %.2f. Dengan tingkat kepercayaan %.0f%%, rata-rata populasi diperkirakan berada antara %.2f dan %.2f. Artinya: jika pengambilan sampel diulang berkali-kali dan interval dihitung dengan cara yang sama, sekitar %.0f%% interval itu akan memuat rata-rata populasi yang sebenarnya.",
              input$var_y, h$m, h$cl * 100, h$bawah, h$atas, h$cl * 100))
  })
  
  # ===== Halaman 5 =====
  data_grup <- reactive({
    d <- data_aktif(); req(input$var_y, input$var_g)
    x <- d[[input$var_y]]; g <- factor(d[[input$var_g]])
    ok <- !is.na(x) & !is.na(g)
    validate(need(nlevels(droplevels(g[ok])) == 2,
                  "Variabel kelompok harus punya tepat 2 kategori."))
    list(x = x[ok], g = droplevels(g[ok]))
  })
  
  output$rumusan_h <- renderUI({
    req(input$var_y, input$var_g)
    tagList(
      p(strong("H0: "), sprintf("rata-rata %s sama di kedua kelompok %s (\u03bc1 = \u03bc2).",
                                input$var_y, input$var_g)),
      p(strong("H1: "), "rata-rata keduanya berbeda (\u03bc1 \u2260 \u03bc2), uji dua arah.")
    )
  })
  
  output$tabel_asumsi <- renderTable({
    dg <- data_grup()
    do.call(rbind, lapply(levels(dg$g), function(l) {
      x <- dg$x[dg$g == l]
      pv <- if (length(x) >= 3 && length(x) <= 5000) shapiro.test(x)$p.value else NA
      data.frame(Kelompok = l, n = length(x),
                 Mean = sprintf("%.2f", mean(x)), SD = sprintf("%.2f", sd(x)),
                 Shapiro_p = ifelse(is.na(pv), "-", fmt_p(pv)))
    }))
  })
  
  hasil_varians <- reactive({
    dg <- data_grup()
    z   <- abs(dg$x - ave(dg$x, dg$g, FUN = median))
    sds <- tapply(dg$x, dg$g, sd)
    list(p = anova(lm(z ~ dg$g))[["Pr(>F)"]][1], rasio = max(sds) / min(sds))
  })
  
  output$tabel_varians <- renderTable({
    h <- hasil_varians()
    data.frame(Pemeriksaan = c("Uji Brown-Forsythe (p-value)", "Rasio SD terbesar : terkecil"),
               Nilai = c(fmt_p(h$p), sprintf("%.2f", h$rasio)))
  })
  
  output$qq_grup <- renderPlot({
    dg <- data_grup()
    par(mfrow = c(1, 2))
    for (l in levels(dg$g)) {
      x <- dg$x[dg$g == l]
      qqnorm(x, main = l); qqline(x, col = "#E55353", lwd = 2)
    }
  })
  
  output$kesimpulan_asumsi <- renderUI({
    dg <- data_grup(); h <- hasil_varians(); a <- as.numeric(input$alpha)
    p(sprintf("Kelompok terkecil berisi %d pengamatan, sehingga rata-rata sampel diperkirakan mendekati normal (Teorema Limit Pusat). Uji Brown-Forsythe memberi p = %.3f, yang %s taraf signifikansi %.2f, jadi varians kedua kelompok dianggap %s.",
              min(table(dg$g)), h$p, ifelse(h$p < a, "lebih kecil dari", "tidak lebih kecil dari"),
              a, ifelse(h$p < a, "berbeda", "tidak berbeda nyata")))
  })
  
  hasil_uji <- reactive({
    dg <- data_grup(); a <- as.numeric(input$alpha)
    lv <- levels(dg$g)
    x1 <- dg$x[dg$g == lv[1]]; x2 <- dg$x[dg$g == lv[2]]
    w  <- t.test(x1, x2, conf.level = 1 - a)       # Welch (var.equal = FALSE bawaan)
    mw <- wilcox.test(x1, x2, exact = FALSE)
    n1 <- length(x1); n2 <- length(x2)
    sp <- sqrt(((n1 - 1) * var(x1) + (n2 - 1) * var(x2)) / (n1 + n2 - 2))
    list(lv = lv, selisih = mean(x1) - mean(x2), ci = w$conf.int,
         t = unname(w$statistic), df = unname(w$parameter),
         p = w$p.value, p_mw = mw$p.value,
         d = (mean(x1) - mean(x2)) / sp)
  })
  
  output$tabel_uji <- renderTable({
    h <- hasil_uji(); a <- as.numeric(input$alpha)
    data.frame(
      Ukuran = c(paste0("Selisih rata-rata (", h$lv[1], " - ", h$lv[2], ")"),
                 sprintf("Selang kepercayaan %.0f%% untuk selisih", (1 - a) * 100),
                 "Statistik t (Welch)", "Derajat bebas",
                 "p-value Welch t-test", "p-value Mann-Whitney U (pembanding)",
                 "Cohen's d (ukuran efek)"),
      Nilai  = c(sprintf("%.2f", h$selisih),
                 sprintf("%.2f sampai %.2f", h$ci[1], h$ci[2]),
                 sprintf("%.2f", h$t), sprintf("%.1f", h$df),
                 fmt_p(h$p), fmt_p(h$p_mw), sprintf("%.2f", h$d))
    )
  })
  
  output$keputusan_uji <- renderUI({
    h <- hasil_uji(); a <- as.numeric(input$alpha)
    tolak <- h$p < a
    ukuran <- if (abs(h$d) < 0.2) {
      "sangat kecil"
    } else if (abs(h$d) < 0.5) {
      "kecil"
    } else if (abs(h$d) < 0.8) {
      "sedang"
    } else {
      "besar"
    }
    tagList(
      p(strong("Keputusan: "),
        sprintf("Welch t-test memberi p = %s, %s \u03b1 = %.2f, sehingga H0 %s. %s",
                fmt_p(h$p), ifelse(tolak, "lebih kecil dari", "tidak lebih kecil dari"), a,
                ifelse(tolak, "ditolak", "tidak ditolak"),
                ifelse(tolak,
                       "Ada cukup bukti bahwa rata-rata kedua kelompok berbeda.",
                       "Belum cukup bukti bahwa rata-rata kedua kelompok berbeda (ini tidak berarti pasti sama)."))),
      p(sprintf("Ukuran efek (Cohen's d = %.2f) tergolong %s. Mann-Whitney U sebagai pembanding memberi p = %s, %s.",
                h$d, ukuran, fmt_p(h$p_mw),
                ifelse((h$p_mw < a) == tolak,
                       "kesimpulannya searah dengan Welch",
                       "kesimpulannya berbeda dari Welch sehingga perlu ditafsirkan hati-hati"))),
      p(em("Catatan: data harian berurutan saling berkaitan, sehingga p-value cenderung lebih kecil dari yang seharusnya. Mann-Whitney membandingkan sebaran secara umum, bukan khusus rata-rata."))
    )
  })
}

shinyApp(ui, server)