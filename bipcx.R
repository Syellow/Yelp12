require(RColorBrewer)
require(FactoMineR)
require(factoextra)
require(highcharter)

Set3 = brewer.pal(12, "Set3") 
bipcx = function(pcx, d1, d2, nvar, nobs, t1="", t2="",
                 main="Principle Component Anaylysis", 
                 obs='obs.', col.o='gold', ratio=0.7) {
  dfvar = pcx$var$coord %>% 
  {data.frame(name=rownames(.),x=.[,d1],y=.[,d2] )}
  dfvar = head(dfvar[order(-rowSums(pcx$var$cos2[,c(d1,d2)])),], nvar)
  dfobs = pcx$ind$coord %>% 
  {data.frame(name=rownames(.),x=.[,d1],y=.[,d2])}
  dfobs = head(dfobs[order(-rowSums(pcx$ind$cos2[,c(d1,d2)])),], nobs)
  dfvar[-1] = ratio*dfvar[-1]*max(abs(dfobs[,-1]))/max(abs(dfvar[-1])) 
  lsvar = dfvar %>% group_by_("name") %>%
    do(data = list(c(0, 0), c(.$x, .$y))) %>% list_parse()
  highchart() %>%
    hc_colors(substr(Set3, 0, 7)) %>% 
    hc_plotOptions( 
      line = list(
        marker=list(enabled=F),
        tooltip=list(pointFormat="{series.name}")),
      scatter = list(marker=list(radius=4, symbol="circle"))
    ) %>%
    hc_tooltip(headerFormat = "",valueDecimals=1,borderWidth=2) %>%
    hc_add_series_list(lsvar) %>%
    hc_add_series(data = list_parse(dfobs), 
                  name = obs, type = "scatter", color = hex_to_rgba(col.o, 0.65),
                  tooltip = list(headerFormat="",pointFormat="{point.name}")) %>%
    hc_chart(zoomType = "xy") %>%
    hc_add_theme(hc_theme_flatdark()) %>% 
    hc_title(text=main) %>% 
    hc_xAxis(title=list(
      text=sprintf("dim%d(%.2f%%) %s",d1,pcx$eig[d1,2],t1),
      style=list(color="white")))%>% 
    hc_yAxis(title=list(
      text=sprintf("dim%d(%.2f%%) %s",d2,pcx$eig[d2,2],t2),
      style=list(color="white"))) %>% 
    hc_legend(align="right", verticalAlign="top",layout="vertical")
}
