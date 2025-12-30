library(shiny)
library(visNetwork)
library(jsonlite)
library(dplyr)

# --- 1. DATA LOADING ---
# Load the standard JSON file directly
json_data <- jsonlite::read_json("posts_data.json", simplifyVector = TRUE)

# Prepare Nodes
nodes <- json_data %>%
  select(id, title, subtitle, summary, sourceUrl, linkedinUrl, keywords) %>%
  rename(label = title) %>%
  mutate(
    # Visual properties
    shape = "dot",
    # FIX: Wrap the color list in another list() or use I() to prevent incorrect recycling
    # This ensures every node gets this specific list of color properties
    color = list(list(background = "#9333ea", border = "#7e22ce", highlight = "#32cd32")), # highlight orange
    font = list(list(color = "#e5e5e5", size = 16, face = "sans")),
    size = 20 + (sapply(keywords, length) * 1.5), # Size by connectivity
    shadow = TRUE,
    # Tooltip (hover) - with inline styles for dark theme
    title = paste0("<div style='background-color: #1a1a1a; color: #e5e5e5; padding: 10px 14px; border-radius: 6px; border: 1px solid #000000; font-family: sans-serif; box-shadow: 0 4px 12px rgba(0,0,0,0.5);'><b>", label, "</b><br>", subtitle, "</div>") 
  )

# Prepare Edges
edges <- data.frame(from = character(), to = character(), weight = numeric(), stringsAsFactors = FALSE)

# Helper to normalize keywords
normalize <- function(x) { tolower(gsub("[\\s-]", "", x)) }

n <- nrow(nodes)
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    k1 <- nodes$keywords[[i]]
    k2 <- nodes$keywords[[j]]
    
    # Find shared keywords
    if (is.null(k1) || is.null(k2)) next
    
    shared <- intersect(normalize(k1), normalize(k2))
    
    if (length(shared) > 0) {
      edges <- rbind(edges, data.frame(
        from = nodes$id[i],
        to = nodes$id[j],
        weight = length(shared)
      ))
    }
  }
}

# FIX: Correctly replicate the list object for every row
# We create a list of lists, one for each row in 'edges'
# Highlight color set to orange (#fb923c)
edges$color <- rep(list(list(color = "#333333", highlight = "#32cd32", opacity = 0.4)), nrow(edges))
edges$width <- edges$weight 

# --- 2. UI ---
ui <- fillPage(
  tags$head(
    # Dark Mode CSS
    tags$style(HTML("
      body { background-color: #111111; color: #e5e5e5; font-family: sans-serif; }
      .vis-network { outline: none; }
      
      /* Tooltip styling for node hover - hide default tooltip */
      .vis-tooltip {
        display: none !important;
        visibility: hidden !important;
        opacity: 0 !important;
      }
      /* Hide default tooltip */
      div.vis-tooltip {
        display: none !important;
        visibility: hidden !important;
      }
      
      /* Modal Customization - Force overrides with !important */
      .modal-content { 
        background-color: #1a1a1a !important; 
        border: 1px solid #444 !important; 
        color: #e5e5e5 !important; 
      }
      .modal-header { 
        border-bottom: 1px solid #333 !important; 
      }
      .modal-footer { 
        border-top: 1px solid #333 !important;
        text-align: center !important;
        justify-content: center !important;
      }
      .modal-title { 
        color: #ffffff !important; 
        font-weight: bold;
      }
      .modal-body {
        color: #e5e5e5 !important;
      }
      .modal-body h4 { 
        color: #cccccc !important; 
      }
      .modal-body p { 
        color: #dddddd !important; 
      }
      .close { 
        color: #ffffff !important; 
        text-shadow: none !important; 
        opacity: 0.8 !important; 
      } 
      .close:hover { 
        opacity: 1 !important; 
      }
      .btn-default { 
        background-color: #333333 !important; 
        color: #ffffff !important; 
        border: 1px solid #444444 !important; 
      }
      .btn-default:hover { 
        background-color: #444444 !important; 
        color: #ffffff !important; 
      }
      a { color: #22d3ee !important; }
    ")),
    # JavaScript to hide default tooltips
    tags$script(HTML("
      $(document).ready(function() {
        var style = document.createElement('style');
        style.innerHTML = '.vis-tooltip { display: none !important; visibility: hidden !important; }';
        document.head.appendChild(style);
        
        // MutationObserver to hide default tooltips as they're created
        var observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            mutation.addedNodes.forEach(function(node) {
              if (node.classList && node.classList.contains('vis-tooltip')) {
                node.style.setProperty('display', 'none', 'important');
                node.style.setProperty('visibility', 'hidden', 'important');
              }
            });
          });
        });
        observer.observe(document.body, { childList: true, subtree: true });
      });
    "))
  ),
  
  # Loading screen overlay with circular progress
  div(id = "loading-screen",
      style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: #111111; z-index: 2000; display: flex; flex-direction: column; justify-content: center; align-items: center;",
      # SVG circular progress
      HTML('
        <svg width="120" height="120" viewBox="0 0 120 120">
          <circle cx="60" cy="60" r="50" fill="none" stroke="#333" stroke-width="8"/>
          <circle id="progress-circle" cx="60" cy="60" r="50" fill="none" stroke="#e31b1b" stroke-width="8" 
                  stroke-linecap="round" stroke-dasharray="314.159" stroke-dashoffset="314.159"
                  transform="rotate(-90 60 60)"/>
          <text id="progress-text" x="60" y="65" text-anchor="middle" fill="#e5e5e5" font-size="20" font-family="sans-serif">0%</text>
        </svg>
      '),
      p("Loading Fendi's 2025 LinkedIn Posts Graph...", style = "color: #e5e5e5; margin-top: 20px; font-family: sans-serif;")
  ),
  
  
  # Main Graph
  visNetworkOutput("network_graph", width = "100%", height = "100%")
)

# --- 3. SERVER ---
server <- function(input, output, session) {
  
  # Render Graph
  output$network_graph <- renderVisNetwork({
    # Create node lookup for tooltips
    node_lookup_json <- jsonlite::toJSON(
      nodes %>% select(id, label, subtitle),
      auto_unbox = TRUE
    )
    
    visNetwork(nodes, edges) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = FALSE),
                 nodesIdSelection = FALSE) %>%
      visInteraction(tooltipDelay = 0, hideEdgesOnDrag = TRUE, tooltipStyle = 'visibility: hidden;') %>%
      visPhysics(stabilization = TRUE, 
                 solver = "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -100, centralGravity = 0.005, springLength = 100, springConstant = 0.08)) %>%
      visEvents(
        doubleClick = "function(nodes) { Shiny.onInputChange('double_click_node_id', nodes.nodes[0]); }",
        hoverNode = sprintf("function(params) {
          var nodeLookup = %s;
          var nodeId = params.node;
          var nodeData = nodeLookup.find(function(n) { return n.id === nodeId; });
          if (!nodeData) return;
          
          // Remove any existing custom tooltip
          var existingTooltip = document.getElementById('custom-tooltip');
          if (existingTooltip) existingTooltip.remove();
          
          // Create custom tooltip
          var tooltip = document.createElement('div');
          tooltip.id = 'custom-tooltip';
          tooltip.style.cssText = 'position: fixed; background-color: #1a1a1a; color: #e5e5e5; padding: 10px 14px; border-radius: 6px; border: 1px solid #000000; font-family: sans-serif; font-size: 14px; box-shadow: 0 4px 12px rgba(0,0,0,0.5); z-index: 9999; pointer-events: none; max-width: 350px;';
          tooltip.innerHTML = '<b>' + (nodeData.label || '') + '</b><br>' + (nodeData.subtitle || '');
          
          // Position tooltip using page coordinates from the original event
          var pageX = params.event.pointers[0].pageX;
          var pageY = params.event.pointers[0].pageY;
          tooltip.style.left = (pageX + 15) + 'px';
          tooltip.style.top = (pageY + 15) + 'px';
          
          document.body.appendChild(tooltip);
        }", node_lookup_json),
        blurNode = "function(params) {
          var existingTooltip = document.getElementById('custom-tooltip');
          if (existingTooltip) existingTooltip.remove();
        }",
        stabilizationIterationsDone = "function() {
          // Set progress to 100%
          var circle = document.getElementById('progress-circle');
          var text = document.getElementById('progress-text');
          if (circle && text) {
            circle.style.strokeDashoffset = 0;
            text.textContent = '100%';
          }
          
          // Fade out loading screen
          var loadingScreen = document.getElementById('loading-screen');
          if (loadingScreen) {
            setTimeout(function() {
              loadingScreen.style.opacity = '0';
              loadingScreen.style.transition = 'opacity 0.5s ease';
              setTimeout(function() {
                loadingScreen.style.display = 'none';
              }, 500);
            }, 300);
          }
        }",
        stabilizationProgress = "function(params) {
          var progress = params.iterations / params.total;
          var percent = Math.round(progress * 100);
          var circle = document.getElementById('progress-circle');
          var text = document.getElementById('progress-text');
          if (circle && text) {
            var circumference = 314.159;
            var offset = circumference * (1 - progress);
            circle.style.strokeDashoffset = offset;
            text.textContent = percent + '%';
          }
        }"
      ) %>%
      visLayout(randomSeed = 42)
  })
  
  # Handle Double Click (Show Modal)
  observeEvent(input$double_click_node_id, {
    node_id <- input$double_click_node_id
    req(node_id)
    
    selected_node <- nodes %>% filter(id == node_id)
    
    showModal(modalDialog(
      title = HTML(paste0("<div style='display:flex; align-items:center; gap:10px;'><div style='background:#9333ea; width:10px; height:10px; border-radius:50%;'></div>", 
                          selected_node$label, "</div>")),
      div(
        h4(selected_node$subtitle, style = "margin-top: 0;"),
        a(href = selected_node$sourceUrl, target = "_blank", class = "btn btn-default", "View Source"),
        a(href = selected_node$linkedinUrl, target = "_blank", class = "btn btn-default", "View on LinkedIn"),
        hr(style = "border-color: #333;"),
        div(style = "display: flex; gap: 5px; flex-wrap: wrap;",
            lapply(unlist(selected_node$keywords), function(kw) {
              tags$span(style = "background: #333; padding: 2px 8px; border-radius: 10px; font-size: 0.8em; color: #bbb;", paste0("#", kw))
            })
        ),
        br(),
        p(selected_node$summary, style = "line-height: 1.6; white-space: pre-wrap;")
      ),
      footer = tagList(
        modalButton("Close")
      ),
      easyClose = TRUE,
      size = "l"
    ))
  })
}

shinyApp(ui, server)