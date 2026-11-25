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
fmt_num <- function(x) ifelse(x == round(x),
                              formatC(x, format = "f", digits = 0, big.mark = ".", decimal.mark = ","),
                              formatC(x, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))
fmt_df <- function(df) { df[] <- lapply(df, function(k) if (is.numeric(k)) fmt_num(k) else k); df }
kpi  <- function(...) layout_columns(fill = FALSE, ...)
poin <- function(...) tags$ol(class = "gs-poin", lapply(list(...), tags$li))

# ---- Tampilan (desain GowesStat) ----
tema <- bs_theme(version = 5, primary = "#0101B7", secondary = "#8DCCF1",
                 bg = "#FCF8FF", fg = "#17173D",
                 base_font = font_google("Plus Jakarta Sans", local = FALSE),
                 heading_font = font_google("Lexend", local = FALSE))

css <- "
.bslib-sidebar-layout>.sidebar{background:#fff;box-shadow:0 1px 8px rgba(1,1,183,.06)}
.gs-brand{display:flex;align-items:center;gap:10px;margin-bottom:18px}
.gs-brand b{font-family:Lexend,sans-serif;font-size:20px;color:#0101B7;line-height:1}
.gs-brand small{display:block;color:#1C6586;font-size:11px;font-weight:600}
.gs-menu a{display:block;padding:10px 14px;margin-bottom:4px;border-radius:12px;color:#17173D;font-weight:600;text-decoration:none;cursor:pointer}
.gs-menu a:hover{background:#EFECFF}
.gs-menu a.active{background:#0101B7;color:#fff;box-shadow:0 4px 12px rgba(1,1,183,.18)}
.gs-config{background:#F5F2FF;border-radius:16px;padding:12px;margin-top:14px}
.gs-tip{background:#DDF1FD;border-radius:14px;padding:12px;font-size:13px;margin-top:14px}
.gs-head h1{font-family:Lexend,sans-serif;font-size:30px;font-weight:600;color:#0101B7;margin:0}
.gs-head p{color:#454555;margin:4px 0 18px}
.card{border:0;border-radius:20px;box-shadow:0 2px 14px rgba(1,1,183,.07)}
.card-header{background:transparent;border:0;font-family:Lexend,sans-serif;font-weight:600;color:#0101B7;font-size:17px}
.bslib-value-box{border-radius:20px!important;background:#fff!important;color:#0101B7!important;box-shadow:0 2px 14px rgba(1,1,183,.07)}
.gs-callout{border-left:6px solid #FFD166}
.gs-btn{display:inline-block;background:#0101B7;color:#fff!important;padding:10px 18px;border-radius:12px;font-weight:600;text-decoration:none;cursor:pointer}
th{color:#0101B7}
.table thead th{background:#EFECFF;color:#0101B7;font-weight:600;border:0;padding:10px}
.table tbody tr:nth-child(even){background:#F8F6FF}
.gs-badge{display:inline-block;background:#FFDF9B;color:#17173D;border-radius:999px;padding:3px 12px;font-size:12px;font-weight:700;margin-bottom:8px}
.gs-poin{list-style:none;counter-reset:n;padding:0;margin:0}
.gs-poin li{counter-increment:n;position:relative;padding:4px 0 10px 40px}
.gs-poin li:before{content:counter(n);position:absolute;left:0;top:2px;width:26px;height:26px;border-radius:50%;background:#0101B7;color:#fff;text-align:center;line-height:26px;font-size:13px;font-weight:700}
.gs-config .shiny-input-container{margin-bottom:6px}
.gs-config label{font-size:13px}
.gs-menu a{padding:8px 14px}
.gs-brand{margin-bottom:10px}
"

js <- "$(document).on('click','[data-page]',function(){var p=$(this).data('page');
$('.gs-menu a').removeClass('active').filter('[data-page=\"'+p+'\"]').addClass('active');
Shiny.setInputValue('halaman_pilih',p,{priority:'event'});});"

logo <- HTML('<svg width="34" height="34" viewBox="0 0 34 34"><circle cx="17" cy="17" r="15" fill="none" stroke="#0101B7" stroke-width="3"/><rect x="9" y="19" width="4" height="7" rx="1" fill="#8DCCF1"/><rect x="15" y="14" width="4" height="12" rx="1" fill="#0101B7"/><rect x="21" y="9" width="4" height="17" rx="1" fill="#FFD166"/></svg>')

kepala <- function(judul, sub) div(class = "gs-head", h1(judul), p(sub))
lanjut <- function(teks, hal) p(tags$a(class = "gs-btn", `data-page` = hal, teks))
menu_item <- function(teks, hal, aktif = FALSE)
  tags$a(class = if (aktif) "active", `data-page` = hal, teks)

ui <- page_sidebar(
  title = NULL, theme = tema, fillable = FALSE, window_title = "GowesStat",
  tags$head(tags$style(HTML(css)), tags$script(HTML(js))),
  sidebar = sidebar(
    width = 290,
    div(class = "gs-brand", logo, div(tags$b("GowesStat"), tags$small("Statistika Interaktif"))),
    div(class = "gs-menu",
        menu_item("Ringkasan Dataset", "ringkasan", TRUE),
        menu_item("Statistik Deskriptif", "deskriptif"),
        menu_item("Probabilitas & Distribusi", "probabilitas"),
        menu_item("Estimasi Parameter", "estimasi"),
        menu_item("Uji Hipotesis", "uji")),
    div(class = "gs-config",
        strong("Konfigurasi data"),
        radioButtons("sumber", NULL,
                     c("Data contoh (Bike Sharing)" = "contoh", "Unggah CSV saya" = "unggah")),
        conditionalPanel("input.sumber == 'unggah'",
                         fileInput("berkas", "Pilih file CSV", accept = ".csv")),
        selectInput("var_y", "Variabel numerik (dianalisis)", choices = NULL),
        selectInput("var_g", "Variabel kelompok (2 kategori)", choices = NULL)),
    div(class = "gs-tip", strong("Tips belajar"), br(),
        "Gunakan variabel numerik untuk membandingkan rata-rata antarkelompok.")
  ),
  navset_hidden(
    id = "halaman",
    
    nav_panel_hidden("ringkasan",
                     kepala("Ringkasan Dataset", "Gambaran awal struktur data yang dianalisis."),
                     uiOutput("kotak"),
                     card(card_header("Cuplikan data"),
                          div(style = "overflow-x:auto;", tableOutput("tabel_awal"))),
                     card(card_header("Pemeriksaan variabel"), tableOutput("tabel_variabel")),
                     lanjut("Mulai analisis deskriptif", "deskriptif")),
    
    nav_panel_hidden("deskriptif",
                     kepala("Statistik Deskriptif", "Ukuran pemusatan dan penyebaran data, secara keseluruhan dan per kelompok."),
                     uiOutput("kpi_desk"),
                     layout_columns(fill = FALSE,
                                    card(card_header("Seluruh data"), tableOutput("tabel_desk")),
                                    card(card_header("Per kelompok"), tableOutput("tabel_desk_grup"))),
                     layout_columns(fill = FALSE,
                                    card(card_header("Histogram"), plotOutput("hist_y")),
                                    card(card_header("Boxplot per kelompok"), plotOutput("box_y"))),
                     card(class = "gs-callout", card_header("Interpretasi"), uiOutput("interpretasi_desk")),
                     lanjut("Lanjut ke Probabilitas & Distribusi", "probabilitas")),
    
    nav_panel_hidden("probabilitas",
                     kepala("Probabilitas & Distribusi", "Seberapa cocok data dengan distribusi normal, dan peluang kejadian tertentu."),
                     uiOutput("kpi_prob"),
                     layout_columns(fill = FALSE,
                                    card(card_header("Histogram + kurva normal"), plotOutput("hist_norm")),
                                    card(card_header("Q-Q plot normal"), plotOutput("qq_y"))),
                     card(card_header("Ukuran bentuk sebaran"), tableOutput("tabel_normal")),
                     card(card_header("Hitung peluang"),
                          layout_columns(fill = FALSE,
                                         numericInput("ambang", "Ambang (x)", value = 0),
                                         selectInput("arah", "Peluang", c("lebih dari ambang" = "lebih",
                                                                          "kurang dari ambang" = "kurang"))),
                          tableOutput("tabel_peluang"), uiOutput("interpretasi_peluang")),
                     card(card_header("Peluang hari ramai menurut kategori"),
                          p("Hari ramai = nilai variabel numerik lebih dari ambang di atas."),
                          selectInput("var_k", "Kategori pembanding", choices = NULL),
                          tableOutput("tabel_bersyarat"), uiOutput("interpretasi_bersyarat")),
                     lanjut("Lanjut ke Estimasi Parameter", "estimasi")),
    
    nav_panel_hidden("estimasi",
                     kepala("Estimasi Parameter", "Memperkirakan rata-rata populasi dengan interval kepercayaan."),
                     uiOutput("kpi_est"),
                     card(card_header("Interval kepercayaan rata-rata"),
                          selectInput("cl", "Tingkat kepercayaan",
                                      c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99), selected = 0.95),
                          tableOutput("tabel_ci"),
                          p(em("Catatan: interval ini mengasumsikan pengamatan saling bebas. Data harian berurutan biasanya saling berkaitan, sehingga interval cenderung lebih sempit dari yang seharusnya."))),
                     card(card_header("Perbandingan lebar interval (90%, 95%, 99%)"), plotOutput("plot_ci", height = "220px")),
                     card(class = "gs-callout", card_header("Interpretasi"), uiOutput("interpretasi_ci")),
                     lanjut("Lanjut ke Uji Hipotesis", "uji")),
    
    nav_panel_hidden("uji",
                     kepala("Uji Hipotesis", "Apakah rata-rata dua kelompok berbeda secara nyata?"),
                     uiOutput("kpi_uji"),
                     card(card_header("Rumusan hipotesis"), uiOutput("rumusan_h"),
                          selectInput("alpha", "Taraf signifikansi (\u03b1)",
                                      c("0.01", "0.05", "0.10"), selected = "0.05")),
                     card(card_header("Langkah 1: periksa asumsi"),
                          tableOutput("tabel_asumsi"), tableOutput("tabel_varians"),
                          plotOutput("qq_grup", height = "300px"), uiOutput("kesimpulan_asumsi")),
                     card(class = "gs-callout", card_header("Langkah 2: uji dan keputusan"),
                          p("Uji utama: Welch t-test, karena tidak mengasumsikan varians kedua kelompok sama dan aman untuk kelompok berukuran berbeda. Mann-Whitney U dipakai sebagai pembanding."),
                          tableOutput("tabel_uji"), uiOutput("keputusan_uji")))
  )
)

# ---- Logika ----
server <- function(input, output, session) {
  
  observeEvent(input$halaman_pilih, nav_select("halaman", input$halaman_pilih))
  
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
  
  output$tabel_awal <- renderTable({
    d <- data_aktif()
    if (all(c("dteday", "cuaca", "hari", "suhu_c", "cnt") %in% names(d))) {
      d <- data.frame(Tanggal = d$dteday, Cuaca = d$cuaca, Hari = d$hari,
                      Suhu_C = formatC(d$suhu_c, format = "f", digits = 1, decimal.mark = ","),
                      Sewa = fmt_num(d$cnt))
    }
    head(d, 8)
  })
  
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
    fmt_df(ringkas(d[[input$var_y]]))
  })
  
  output$tabel_desk_grup <- renderTable({
    d <- data_aktif(); req(input$var_y, input$var_g)
    g <- factor(d[[input$var_g]])
    fmt_df(do.call(rbind, lapply(levels(g), function(l)
      cbind(Kelompok = l, ringkas(d[[input$var_y]][which(g == l)])))))
  })
  
  output$hist_y <- renderPlot({
    d <- data_aktif(); req(input$var_y)
    hist(d[[input$var_y]], col = "#8DCCF1", border = "white",
         main = NULL, xlab = input$var_y, ylab = "Frekuensi")
  })
  
  output$box_y <- renderPlot({
    d <- data_aktif(); req(input$var_y, input$var_g)
    boxplot(d[[input$var_y]] ~ factor(d[[input$var_g]]),
            col = "#8DCCF1", xlab = input$var_g, ylab = input$var_y)
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
    poin(
      sprintf("Rata-rata %s adalah %s dan mediannya %s, sehingga sebaran data %s.",
              input$var_y, fmt_num(m), fmt_num(md), bentuk),
      sprintf("Simpangan baku %s berarti nilai umumnya menyimpang sekitar %s dari rata-rata.",
              fmt_num(s), fmt_num(s)),
      sprintf("Data berkisar dari %s sampai %s. Bandingkan antarkelompok lewat tabel dan boxplot di atas.",
              fmt_num(min(x)), fmt_num(max(x))))
  })
  
  # ===== Halaman 3 =====
  # ambang awal = kuartil 3 dari variabel terpilih
  observeEvent(input$var_y, {
    req(input$var_y)
    x <- na.omit(data_aktif()[[input$var_y]])
    updateNumericInput(session, "ambang",
                       value = as.numeric(round(quantile(x, .75))))
  })
  
  output$hist_norm <- renderPlot({
    d <- data_aktif(); req(input$var_y)
    x <- na.omit(d[[input$var_y]]); m <- mean(x); s <- sd(x)
    hist(x, freq = FALSE, col = "#8DCCF1", border = "white",
         main = NULL, xlab = input$var_y, ylab = "Kepadatan")
    curve(dnorm(x, m, s), add = TRUE, lwd = 2, col = "#0101B7")
  })
  
  output$qq_y <- renderPlot({
    d <- data_aktif(); req(input$var_y)
    x <- na.omit(d[[input$var_y]])
    qqnorm(x, main = NULL); qqline(x, col = "#0101B7", lwd = 2)
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
      Nilai  = fmt_num(c(h$n, h$m, h$se, h$t, h$bawah, h$atas))
    )
  })
  
  output$interpretasi_ci <- renderUI({
    h <- hitung_ci()
    poin(
      sprintf("Estimasi titik rata-rata %s adalah %s.", input$var_y, fmt_num(h$m)),
      sprintf("Dengan tingkat kepercayaan %.0f%%, rata-rata populasi diperkirakan berada antara %s dan %s.",
              h$cl * 100, fmt_num(h$bawah), fmt_num(h$atas)),
      sprintf("Artinya: jika pengambilan sampel diulang berkali-kali dengan cara yang sama, sekitar %.0f%% interval akan memuat rata-rata populasi yang sebenarnya.",
              h$cl * 100))
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
                 Mean = fmt_num(mean(x)), SD = fmt_num(sd(x)),
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
      qqnorm(x, main = l); qqline(x, col = "#0101B7", lwd = 2)
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
      div(span(class = "gs-badge", ifelse(tolak, "H0 ditolak", "H0 tidak ditolak"))),
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
  
  # ===== Kartu angka & grafik tambahan =====
  kartu <- function(judul, nilai) value_box(judul, nilai)
  
  output$kpi_desk <- renderUI({
    d <- data_aktif(); req(input$var_y)
    x <- na.omit(d[[input$var_y]])
    sel <- tryCatch({
      dg <- data_grup(); m <- tapply(dg$x, dg$g, mean)
      kartu(paste0("Selisih rata-rata (", names(m)[1], " - ", names(m)[2], ")"),
            fmt_num(m[[1]] - m[[2]]))
    }, error = function(e) NULL)
    kpi(kartu("Rata-rata", fmt_num(mean(x))), kartu("Median", fmt_num(median(x))),
        kartu("Simpangan baku", fmt_num(sd(x))), sel)
  })
  
  output$kpi_prob <- renderUI({
    d <- data_aktif(); req(input$var_y, input$ambang)
    x <- na.omit(d[[input$var_y]])
    pv <- if (length(x) >= 3 && length(x) <= 5000) shapiro.test(x)$p.value else NA
    kpi(kartu("Rata-rata (\u03bc)", fmt_num(mean(x))),
        kartu("Simpangan baku (\u03c3)", fmt_num(sd(x))),
        kartu("Ambang hari ramai (x)", fmt_num(input$ambang)),
        kartu("Shapiro-Wilk p-value", ifelse(is.na(pv), "-", fmt_p(pv))))
  })
  
  output$kpi_est <- renderUI({
    h <- hitung_ci()
    kpi(kartu("Estimasi titik", fmt_num(h$m)), kartu("Galat baku (SE)", fmt_num(h$se)),
        kartu("Margin galat (\u00b1)", fmt_num(h$t * h$se)))
  })
  
  output$kpi_uji <- renderUI({
    h <- hasil_uji()
    kpi(kartu("Selisih rata-rata", fmt_num(h$selisih)), kartu("p-value Welch", fmt_p(h$p)),
        kartu("Cohen's d", fmt_num(h$d)))
  })
  
  output$plot_ci <- renderPlot({
    d <- data_aktif(); req(input$var_y)
    x <- na.omit(d[[input$var_y]]); n <- length(x); m <- mean(x); se <- sd(x) / sqrt(n)
    cls <- c(0.90, 0.95, 0.99); tk <- qt(1 - (1 - cls) / 2, n - 1)
    lo <- m - tk * se; hi <- m + tk * se
    par(mar = c(4, 4, 1, 1))
    plot(NA, xlim = range(lo, hi), ylim = c(0.5, 3.5), yaxt = "n", ylab = "",
         xlab = input$var_y, bty = "n")
    axis(2, at = 1:3, labels = c("90%", "95%", "99%"), las = 1, tick = FALSE)
    segments(lo, 1:3, hi, 1:3, lwd = 10, col = c("#8DCCF1", "#0101B7", "#FFDF9B"), lend = 1)
    abline(v = m, lty = 2, col = "#454555")
    points(rep(m, 3), 1:3, pch = 21, bg = "white", cex = 1.5)
  })
}

shinyApp(ui, server)
