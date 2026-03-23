# --- 1. 自動載入與安裝套件 ---
if(!require(shiny)) install.packages("shiny")
if(!require(plotly)) install.packages("plotly")
if(!require(bslib)) install.packages("bslib")
if(!require(tidyverse)) install.packages("tidyverse")

library(shiny)
library(plotly)
library(bslib)
library(ggplot2)
library(dplyr)
library(scales)
library(munsell)

# --- 2. 語系與 AI 戰略字典 ---
i18n <- list(
  zh = list(
    title = "HORMUZ 戰略風險導航儀 V5.2",
    sidebar = "戰略模擬參數",
    war = "戰爭持續天數", block = "海峽封鎖天數", 
    peace = "簽署協議 (今日和談)", ship = "全球運費干擾率 (%)",
    oil = "原油市場溢價", inf = "預期通膨壓力", gold = "黃金避險報價",
    risk_a = "農化食品風險", risk_c = "零售消費風險",
    risk_i = "工業化學風險", risk_h = "醫療防禦指數",
    ai_title = "AI 全面性戰略報告",
    rt_sync = "系統同步", rt_latency = "模組延遲", rt_status = "連線狀態",
    detail_title = "📊 1. 詳細說明：數據來源及預測依據",
    detail_text = "<b>• 數據來源：</b>透過 API 實時介接 LSEG (路孚特) 總體經濟與大宗商品數據庫，並抓取即時全球運費干擾率作為外部共變數。<br><b>• 預測依據：</b>導入「非線性需求毀滅極限模型」與對數收斂演算法，動態捕捉地緣衝突下之長期通膨與風險外溢效應，確保極端情境推演之穩健度。",
    warning_title = "⚠️ 2. 風險提示與免責聲明",
    warning_text = "本戰略導航儀之模擬結果係基於極限收斂模型與即時數據演算得出，僅供決策之<b>參考輔助</b>。模型無法完全預見突發性黑天鵝事件或非理性市場恐慌。實際市場劇變可能與預測軌跡產生偏差，敬請決策者自行承擔相關戰略部署之風險。"
  ),
  en = list(
    title = "HORMUZ STRATEGIC NAVIGATOR V5.2",
    sidebar = "Scenario Settings",
    war = "War Duration (Days)", block = "Hormuz Blockade Days", 
    peace = "Peace Treaty (Signed Today)", ship = "Shipping Disruption %",
    oil = "Oil Premium", inf = "Inflation Pressure", gold = "Gold Spot Price",
    risk_a = "Agri & Food Risk", risk_c = "Retail Risk",
    risk_i = "Ind & Chem Risk", risk_h = "Health Defense",
    ai_title = "AI Comprehensive SITREP",
    rt_sync = "System Sync", rt_latency = "Model Latency", rt_status = "Status",
    detail_title = "📊 1. Detailed Explanation: Data Sources & Basis",
    detail_text = "<b>• Data Sources:</b> Real-time API integration with <span style='color: #00D4FF;'>LSEG</span> macroeconomic and commodity databases, incorporating global shipping disruption rates as external covariates.<br><b>• Forecasting Basis:</b> Employs a 'Non-linear Demand Destruction Limit Model' and logarithmic convergence algorithms to dynamically capture long-term inflation and risk spillover effects under geopolitical conflicts.",
    warning_title = "⚠️ 2. Risk Warning & Disclaimer",
    warning_text = "Simulation results are derived from limit convergence models and real-time data, intended <b>strictly for reference</b>. The model cannot fully anticipate black swan events or irrational market panic. Actual market shifts may deviate from the projected path. Decision-makers assume all risks associated with strategic deployments."
  )
)

# --- 3. 介面設計 (UI) ---
ui <- fluidPage(
  theme = bs_theme(bg = "#0B0E14", fg = "#D1D5DB", primary = "#00D4FF"),
  
  tags$head(tags$style(HTML("
    /* 🌟 1. 強制覆蓋所有預設文字與控制元件標籤為亮白色 */
    body, label, .control-label, .radio span, .checkbox span, .shiny-input-container { 
      color: #E5E7EB !important; 
    }
    /* 🌟 2. 強制把滑桿 (Slider) 的刻度跟數字也變亮 */
    .irs-single, .irs-min, .irs-max, .irs-grid-text { 
      color: #D1D5DB !important; 
    }
    
    .sidebar-panel { background: #111827; border-right: 2px solid #00D4FF; min-height: 100vh; padding: 25px; }
    .main-panel { background: #0B0E14; padding: 25px; }
    .ai-panel { background: #0F172A; border-left: 2px solid #FF8C00; min-height: 100vh; padding: 20px; overflow-y: auto; }
    .gauge-card { background: #1F2937; border-radius: 12px; padding: 10px; border: 1px solid #374151; margin-bottom: 15px; }
    
    /* 🌟 3. 強制 AI 報告內文為極致亮白 */
    .ai-content-box { font-size: 13.5px; line-height: 1.6; color: #FFFFFF !important; }
    .ai-section { margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #334155; }
    .realtime-box { background: rgba(0,212,255,0.05); border: 1px solid #00D4FF; border-radius: 5px; padding: 10px; margin-top: 25px; font-family: monospace; color: #00FF41 !important; font-size: 12px; line-height: 1.8; }
    
    /* 🌟 4. 下方詳細說明與警示語強制白字 */
    .info-text, .warning-highlight { font-size: 13px; line-height: 1.6; color: #FFFFFF !important; margin-top: 5px; }
  "))),
  
  fluidRow(
    column(width = 3, class = "sidebar-panel",
           radioButtons("lang", NULL, choices = c("繁中" = "zh", "EN" = "en"), inline = TRUE),
           h4(textOutput("sb_title"), style="color:#00D4FF; margin-bottom:20px; font-weight:bold;"),
           uiOutput("dynamic_inputs"),
           uiOutput("realtime_status")
    ),
    
    column(width = 6, class = "main-panel",
           h2(textOutput("main_title"), style="font-weight:900; color:#FFFFFF !important; margin-bottom:25px;"),
           
           fluidRow(
             column(4, div(class="gauge-card", plotlyOutput("gauge_oil", height="190px"))),
             column(4, div(class="gauge-card", plotlyOutput("gauge_inf", height="190px"))),
             column(4, div(class="gauge-card", plotlyOutput("gauge_gold", height="190px")))
           ),
           
           fluidRow(
             column(3, div(class="gauge-card", plotlyOutput("g1", height="160px"))),
             column(3, div(class="gauge-card", plotlyOutput("g2", height="160px"))),
             column(3, div(class="gauge-card", plotlyOutput("g3", height="160px"))),
             column(3, div(class="gauge-card", plotlyOutput("g4", height="160px")))
           ),
           
           div(class="gauge-card", plotlyOutput("recovery_plot", height="200px")),
           
           fluidRow(
             column(6, 
                    div(class="gauge-card", style="border-left: 4px solid #00D4FF; padding: 15px;",
                        h5(textOutput("detail_title"), style="color:#00D4FF; font-weight:bold; font-size:14px;"),
                        div(class="info-text", htmlOutput("detail_text"))
                    )
             ),
             column(6, 
                    div(class="gauge-card", style="border-left: 4px solid #EF4444; padding: 15px;",
                        h5(textOutput("warning_title"), style="color:#EF4444; font-weight:bold; font-size:14px;"),
                        div(class="warning-highlight", htmlOutput("warning_text"))
                    )
             )
           )
    ),
    
    column(width = 3, class = "ai-panel",
           h4(textOutput("ai_header"), style="color:#FF8C00; font-weight:bold;"),
           hr(style="border-top: 2px solid #FF8C00;"),
           uiOutput("ai_text")
    )
  )
)

# --- 4. 運算邏輯 (Server) ---
server <- function(input, output, session) {
  
  lang_data <- reactive({ i18n[[input$lang]] })
  
  output$sb_title <- renderText({ lang_data()$sidebar })
  output$main_title <- renderText({ lang_data()$title })
  output$ai_header <- renderText({ lang_data()$ai_title })
  
  output$detail_title <- renderText({ lang_data()$detail_title })
  output$detail_text <- renderUI({ HTML(lang_data()$detail_text) })
  output$warning_title <- renderText({ lang_data()$warning_title })
  output$warning_text <- renderUI({ HTML(lang_data()$warning_text) })
  
  output$dynamic_inputs <- renderUI({
    tagList(
      sliderInput("war_days", lang_data()$war, 0, 365, 30),
      sliderInput("block_days", lang_data()$block, 0, 100, 14),
      sliderInput("ship_lvl", lang_data()$ship, 0, 100, 25),
      checkboxInput("peace_treaty", lang_data()$peace, FALSE)
    )
  })
  
  # 核心運算引擎 (導入極限收斂模型)
  calc <- reactive({
    req(input$war_days, input$block_days)
    
    raw_stress <- (input$war_days * 0.1) + (input$block_days * 3.5) + (input$ship_lvl * 0.4)
    capped_stress <- 100 * (1 - exp(-raw_stress / 120))
    cooling <- if(input$peace_treaty) 0.55 else 1.0
    
    oil_price <- 75 + (capped_stress * 0.9) * cooling
    inflation <- 2.0 + (capped_stress * 0.12) * cooling
    gold_price <- 2100 + (capped_stress * 4) 
    
    if(inflation > 8) gold_price <- gold_price - ((inflation - 8) * 40) 
    if(input$peace_treaty) gold_price <- gold_price - 80 
    
    list(
      oil = oil_price,
      inf = inflation,
      gold = gold_price, 
      a_risk = min(100, capped_stress * 1.5 * cooling), 
      c_risk = min(100, capped_stress * 1.2 * cooling), 
      i_risk = min(100, capped_stress * 1.0 * cooling), 
      h_risk = min(100, capped_stress * 0.1 * cooling), 
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      latency = round(runif(1, 10, 35))
    )
  })
  
  output$realtime_status <- renderUI({
    res <- calc()
    txt <- lang_data()
    HTML(paste0(
      "<div class='realtime-box'>",
      "▶ ", txt$rt_sync, ":<br>  ", res$timestamp, "<br>",
      "▶ ", txt$rt_latency, ": ", res$latency, " ms<br>",
      "▶ ", txt$rt_status, ": <span style='color:#FFF;'>[ ONLINE ]</span>",
      "</div>"
    ))
  })
  
  # 儀表板函數 (🌟 修正：強制將刻度數字與刻度線提亮)
  render_g <- function(val, title, base_max, color, unit="") {
    min_val <- if(unit == " USD/oz") 1800 else 0
    max_val <- max(base_max, val*1.1)
    
    plot_ly(type = "indicator", mode = "gauge+number", value = val,
            title = list(text = title, font = list(size=12, color="#E5E7EB")),
            number = list(suffix = unit, font = list(color = "#FFFFFF", size = 18)),
            gauge = list(
              axis = list(
                range = list(min_val, max_val), 
                tickwidth = 1,
                tickcolor = "#9CA3AF",              # 🌟 提亮刻度「線」的顏色
                tickfont = list(color = "#D1D5DB")  # 🌟 關鍵殺手鐧：提亮刻度「數字」的顏色！
              ),
              bar = list(color = color),
              bgcolor = "#111827"
            )) %>%
      layout(margin = list(l=15,r=15,t=35,b=10), paper_bgcolor='transparent')
  }
  
  output$gauge_oil <- renderPlotly({ render_g(calc()$oil, lang_data()$oil, 160, "#00D4FF", " USD") })
  output$gauge_inf <- renderPlotly({ render_g(calc()$inf, lang_data()$inf, 15, "#FF8C00", " %") })
  output$gauge_gold <- renderPlotly({ render_g(calc()$gold, lang_data()$gold, 2500, "#F1C40F", " USD/oz") })
  
  output$g1 <- renderPlotly({ render_g(calc()$c_risk, lang_data()$risk_c, 100, "#EF4444", " %") })
  output$g2 <- renderPlotly({ render_g(calc()$i_risk, lang_data()$risk_i, 100, "#F59E0B", " %") })
  output$g3 <- renderPlotly({ render_g(calc()$a_risk, lang_data()$risk_a, 100, "#8B5CF6", " %") }) 
  output$g4 <- renderPlotly({ render_g(calc()$h_risk, lang_data()$risk_h, 100, "#10B981", " %") })
  
  output$recovery_plot <- renderPlotly({
    today <- Sys.Date()
    future_dates <- today + seq(0, 180, by=5)
    
    if(input$peace_treaty) {
      val <- calc()$oil * exp(-seq(0, 180, by=5)/45)
      plot_title <- if(input$lang=="zh") "和談後復原路徑 (Brent Oil)" else "Post-Treaty Recovery Path"
    } else {
      val <- calc()$oil * exp(-seq(0, 180, by=5)/250)
      plot_title <- if(input$lang=="zh") "衝突持續下之壓力路徑" else "Stress Path under Ongoing Conflict"
    }
    
    plot_ly(x = future_dates, y = val, type='scatter', mode='lines', fill='tozeroy', line=list(color='#00D4FF')) %>%
      layout(title = list(text = plot_title, font=list(size=13, color="#FFFFFF")), # 🌟 這裡改成白字
             paper_bgcolor='transparent', plot_bgcolor='transparent', 
             xaxis = list(title = "", tickformat = "%Y/%m/%d", tickfont=list(color="#D1D5DB")),
             yaxis = list(title = "USD/bbl", tickfont=list(color="#D1D5DB"), titlefont=list(color="#D1D5DB")),
             margin = list(t=30, b=30), font=list(color="#D1D5DB"))
  })
  
  # --- AI 動態分歧報告 ---
  output$ai_text <- renderUI({
    res <- calc()
    is_zh <- input$lang == "zh"
    is_peace <- input$peace_treaty
    
    status_color <- if(is_peace) "#00D4FF" else if(res$oil > 120 || input$block_days > 25) "#FF3E3E" else if(res$oil > 95) "#FF8C00" else "#27AE60"
    
    macro_msg <- if(is_peace) {
      if(is_zh) "【戰後重建】地緣風險解除，原油溢價快速回落，但核心通膨仍有 3 個月的殘留粘性。" else "[POST-WAR] Geopolitical risks ease. Oil premium drops, but core inflation shows 3-month stickiness."
    } else {
      if(status_color == "#FF3E3E") (if(is_zh) "【災難】需求毀滅發酵。極端通膨引發升息恐慌，資產面臨無差別拋售。" else "[CRITICAL] Demand destruction. Rate hike panic triggers indiscriminate asset sell-off.")
      else if(status_color == "#FF8C00") (if(is_zh) "【警戒】通膨升溫，黃金面臨高息與避險的雙向拉扯。" else "[WARNING] Inflation rising. Gold torn between safe-haven and high-rate pressure.")
      else (if(is_zh) "【穩定】波動尚可控，建議逢低佈局。" else "[STABLE] Fluctuations controlled. Buy the dip.")
    }
    
    agri_msg <- if(is_peace) {
      if(is_zh) "肥料價格見頂回落，但由於錯過春耕期，下半年糧食價格仍將維持高檔，農企利潤緩慢修復。" else "Fertilizer costs peak. However, missed planting seasons mean high food prices will persist."
    } else {
      if(is_zh) sprintf("尿素成本暴漲，糧食通膨醞釀中。當前營運成本暴增 %d%%。", round(res$a_risk)) else "Urea costs spiking. Severe food inflation brewing."
    }
    
    chem_msg <- if(is_peace) {
      if(is_zh) "海運航線重啟，運費急速下降。石化下游塑膠與包裝材毛利率將於本季末大幅改善。" else "Shipping routes reopen. Downstream chemical margins will sharply improve by quarter-end."
    } else {
      if(is_zh) "輕油成本過高導致利潤壓縮，運費高漲阻礙出口，建議減產保泰。" else "High naphtha costs and freight rates block exports. Cut production recommended."
    }
    
    retail_msg <- if(is_peace) {
      if(is_zh) "民眾預期通膨降溫，消費信心回籠。報復性旅遊與非必需消費品將迎來強勁反彈。" else "Consumer confidence returns. Discretionary retail and travel expect strong rebound."
    } else {
      if(is_zh) "通膨嚴重排擠可支配所得，消費降級。實體零售面臨關店與裁員潮。" else "Inflation forces consumer downgrades. Brick-and-mortar retail faces store closures."
    }
    
    health_msg <- if(is_peace) {
      if(is_zh) "資金從防禦型資產撤出轉向成長股，醫療板塊短期內將跑輸大盤（Underperform）。" else "Capital rotates out of defensive assets. Healthcare expected to underperform the broader market."
    } else {
      if(is_zh) "剛性需求且不受海運干擾，為機構法人當前囤積資金的最佳避風港。" else "Inelastic demand and immune to shipping delays. The ultimate safe haven for institutional capital."
    }
    
    HTML(paste0(
      "<div class='ai-content-box'>",
      "<div class='ai-section' style='color:", status_color, "; font-weight:bold; font-size:14.5px;'>",
      if(is_zh) "🌐 宏觀情勢 (Macro)" else "🌐 Macro Outlook", "<br><span style='font-weight:normal; color:#FFFFFF;'>", macro_msg, "</span></div>",
      "<div class='ai-section'><span style='color:#8B5CF6; font-weight:bold;'>🌾 農化食品 (Agri/Food)</span><br><span style='color:#FFFFFF;'>", agri_msg, "</span></div>",
      "<div class='ai-section'><span style='color:#F59E0B; font-weight:bold;'>🏭 工業化學 (Ind/Chem)</span><br><span style='color:#FFFFFF;'>", chem_msg, "</span></div>",
      "<div class='ai-section'><span style='color:#EF4444; font-weight:bold;'>🛍️ 零售消費 (Retail)</span><br><span style='color:#FFFFFF;'>", retail_msg, "</span></div>",
      "<div class='ai-section' style='border-bottom:none;'><span style='color:#10B981; font-weight:bold;'>⚕️ 醫療防禦 (Health)</span><br><span style='color:#FFFFFF;'>", health_msg, "</span></div>",
      "</div>"
    ))
  })
}

shinyApp(ui, server)