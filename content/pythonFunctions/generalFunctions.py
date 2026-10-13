import os

def makeButtonsFit(sites, generalFormat, identifier, altText=""):
    buttons = ""
    scripts = f"""
                    <script>
                    function toggle{identifier}() {{
                        var imgElement = document.getElementById('{identifier}');
                        if (!document.fullscreenElement) {{
                            imgElement.requestFullscreen();
                        }} else {{
                            document.exitFullscreen();
                        }}
                    }}
                    </script>
    """
    inputDir = "https://raw.githubusercontent.com/CI-CMG/SoundscapesWebsite/refs/heads/main/content/resources"
    initialFile = generalFormat.replace('***', sites[0], 1)
    parts = initialFile.split('***')
    cwd = os.getcwd()
    directory_path = cwd.split("SoundscapesWebsite")[0]+ "SoundscapesWebsite/SoundscapesWebsite/content/resources"
    
    fullFileName = ""
    for root, dirs, files in os.walk(directory_path):
        for file in files:
            if file.startswith(parts[0]) and file.endswith(parts[1]):
                fullFileName = file
                
    path = f'{inputDir}/{fullFileName}'
    path = path.replace("***", sites[0])
    initialImage = f'<img alt="{altText}" src="{path}" width="700" id="{identifier}" onclick="this.requestFullscreen()">'
    
    for site in sites:
        initialFile = generalFormat.replace('***', site, 1)
        parts = initialFile.split('***')
        cwd = os.getcwd()
        directory_path = cwd.split("SoundscapesWebsite")[0]+ "SoundscapesWebsite/SoundscapesWebsite/content/resources"
        
        fullFileName = ""
        for root, dirs, files in os.walk(directory_path):
            for file in files:
                if file.startswith(parts[0]) and file.endswith(parts[1]):
                    fullFileName = file
        
        path = f'{inputDir}/{fullFileName}'

        buttons += f'<button onclick="{site}{identifier}()" style="padding: 10px; color: white; margin: 4px 4px; background-color: #008CBA;text-transform: uppercase;">{site}</button>'
        scripts += f"""
                    <script>
                    function {site}{identifier}() {{
                        var imgElement = document.getElementById('{identifier}');
                        imgElement.src = "{path}";
                    }}
                    </script>
        """
    return buttons + initialImage + scripts

def makeButtons(sites, generalFormat, identifier, altText=""):
    buttons = ""
    scripts = f"""
                    <script>
                    function toggle{identifier}() {{
                        var imgElement = document.getElementById('{identifier}');
                        if (!document.fullscreenElement) {{
                            imgElement.requestFullscreen();
                        }} else {{
                            document.exitFullscreen();
                        }}
                    }}
                    </script>
    """
    inputDir = "https://raw.githubusercontent.com/CI-CMG/SoundscapesWebsite/refs/heads/main/content/resources"
    path = f'{inputDir}/{generalFormat}'
    path = path.replace("***", sites[0])
    initialImage = f'<img alt="{altText}" src="{path}" width="700" id="{identifier}" onclick="this.requestFullscreen()">'
    
    for site in sites:
        path = f'{inputDir}/{generalFormat}'
        path = path.replace("***", site)
        
        othersToLight = ""
        for s in sites:
            if s != site:
                othersToLight += f"""const otherButton{s} = document.getElementById('{s}{identifier}button');
                        otherButton{s}.style.backgroundColor = '#008CBA';"""
        
        initialColor = "#008CBA"
        if site == sites[0]:
            initialColor = "#BA2F00"
            
        #buttons += f'<button id="{site}{identifier}button" onclick="{site}{identifier}()" style="padding: 10px; color: white; margin: 4px 4px; background-color: {initialColor};text-transform: uppercase;">{site}</button>'
        buttons += f'<button id="{site}{identifier}button" onclick="{site}{identifier}()" style="padding: 10px; color: white; margin: 4px 0; background-color: {initialColor}; text-transform: uppercase; width: 100px; display: block;">{site}</button>'

        scripts += f"""
                    <script>
                    function {site}{identifier}() {{
                        var imgElement = document.getElementById('{identifier}');
                        imgElement.src = "{path}";
                        const thisButton = document.getElementById('{site}{identifier}button');
                        thisButton.style.backgroundColor = '#BA2F00';
                        {othersToLight}
                    }}
                    </script>
        """

    # Update the return statement like this:
    #container_start = '<div style="display: flex; flex-direction: column; align-items: flex-start; gap: 8px;">'
    container_start = f'<div style="display: flex; flex-direction: row; align-items: flex-start; gap: 20px;">'

    # Wrap the buttons in their own vertical column
    button_column = f'<div style="display: flex; flex-direction: column;">{buttons}</div>'

    # The image stays as it is
    path = f'{inputDir}/{generalFormat}'.replace("***", sites[0])
    image_html = f'<img alt="{altText}" src="{path}" width="700" id="{identifier}" onclick="this.requestFullscreen()" style="border: 1px solid #ccc;">'

    container_end = '</div>'

    return container_start + button_column + image_html + container_end + scripts
    #return container_start + buttons + container_end + initialImage + scripts
    #return buttons + initialImage + scripts
    
def makeButtonsWithLabels(uniqueImageIDs, buttonLabels, generalFormat, identifier, altTexts=[]):
    buttons = ""
    scripts = f"""
                    <script>
                    function toggle{identifier}() {{
                        var imgElement = document.getElementById('{identifier}');
                        if (!document.fullscreenElement) {{
                            imgElement.requestFullscreen();
                        }} else {{
                            document.exitFullscreen();
                        }}
                    }}
                    </script>
    """
    inputDir = "https://raw.githubusercontent.com/CI-CMG/SoundscapesWebsite/refs/heads/main/content/resources"
    path = f'{inputDir}/{generalFormat}'
    path = path.replace("***", uniqueImageIDs[0])
    initialImage = f'<img alt="{altTexts[0]}" src="{path}" width="700" id="{identifier}" onclick="this.requestFullscreen()">'
    
    for i in range(len(uniqueImageIDs)):
        path = f'{inputDir}/{generalFormat}'
        path = path.replace("***", uniqueImageIDs[i])
        
        othersToLight = ""
        for j in range(len(uniqueImageIDs)):
            if i != j:
                othersToLight += f"""const otherButton{uniqueImageIDs[j]} = document.getElementById('{uniqueImageIDs[j]}{identifier}button');
                        otherButton{uniqueImageIDs[j]}.style.backgroundColor = '#008CBA';"""
        
        initialColor = "#008CBA"
        if i == 0:
            initialColor = "#BA2F00"
            
        buttons += f'<button id="{uniqueImageIDs[i]}{identifier}button" onclick="{uniqueImageIDs[i]}{identifier}()" style="padding: 10px; color: white; margin: 4px 4px; background-color: {initialColor};">{buttonLabels[i]}</button>'
        scripts += f"""
                    <script>
                    function {uniqueImageIDs[i]}{identifier}() {{
                        var imgElement = document.getElementById('{identifier}');
                        imgElement.alt = "{altTexts[i]}"
                        imgElement.src = "{path}";
                        const thisButton = document.getElementById('{uniqueImageIDs[i]}{identifier}button');
                        thisButton.style.backgroundColor = '#BA2F00';
                        {othersToLight}
                    }}
                    </script>
        """
    return buttons + initialImage + scripts
  
def makeImage(imageName, identifier, width=700, altText=""):
    inputDir = "https://raw.githubusercontent.com/CI-CMG/SoundscapesWebsite/refs/heads/main/content/resources"
    path = f'{inputDir}/{imageName}'
    initialImage = f'<img alt="{altText}" src="{path}" width="{width}" id="{identifier}" onclick="this.requestFullscreen()" style="display: block; margin-left: auto; margin-right: auto; border: 1px solid #ccc;">'
    return initialImage

def addPlotly(sourceHTML, site="", identifier=""):
    inputDir = "resources"
    path = f'{inputDir}/{sourceHTML}'
    return f'<iframe src="{path}" width="100%" height="900px" style="border:none id="{site}{identifier}";"></iframe>'
#     return f'''
#             <iframe
#                 src="{sourceHTML}"
#                 name="targetframe{site}{identifier}"
#                 id="{site}{identifier}"
#                 allowTransparency="true"
#                 scrolling="no"
#                 frameborder="0"
#                 width="100%"
#                 height="900px"
#             >
#             </iframe>
# 			'''

def addGirafe(site):
    return f"""
            <iframe
                src="resources/plot_{site}_HMDYearSPLInteractive.html"
                width="100%"
                height="950px"
                frameborder="0"
                scrolling="auto"
            ></iframe>
            <hr style="border: 1px solid black; margin: 10px 0;">
            <iframe
                src="resources/plot_{site}_HMDEffortInteractive.html"
                width="100%"
                height="250px"
                frameborder="0"
                scrolling="auto"
            ></iframe>
          """
  
def embedMapViewer(srcLink):
    return f'<embed src="{srcLink}" style="width:900px; height: 800px;">'
    
def makePlotlyButtonsWithLabels(uniqueIDs, buttonLabels, generalFormat, identifier, title=""):
    buttons = ""
    scripts = ""
    inputDir = "resources" 
    path = f'{inputDir}/{generalFormat}'
    path = path.replace("***", uniqueIDs[0])
    initialIframe = f'<div style="flex-grow: 1;"><iframe id="{identifier}" src="{path}" width="600px" style="border:none;" scrolling="no" onload="resizePlotlyIframe(this)" title="{title}"></iframe></div>'
    
    for i in range(len(uniqueIDs)):
        path = f'{inputDir}/{generalFormat}'
        path = path.replace("***", uniqueIDs[i])
        
        othersToLight = ""
        for j in range(len(uniqueIDs)):
            if i != j:
                othersToLight += f"""const otherButton{uniqueIDs[j]} = document.getElementById('{uniqueIDs[j]}{identifier}button');
                        otherButton{uniqueIDs[j]}.style.backgroundColor = '#008CBA';\n"""
        
        initialColor = "#008CBA"
        if i == 0:
            initialColor = "#BA2F00"
            
        buttons += f'<button id="{uniqueIDs[i]}{identifier}button" onclick="{uniqueIDs[i]}{identifier}()" style="padding: 10px; color: white; margin: 4px 0; background-color: {initialColor}; text-transform: uppercase; width: 100px; display: block;">{buttonLabels[i]}</button>'
        
        scripts += f"""
                    <script>
                    function {uniqueIDs[i]}{identifier}() {{
                        var frameElement = document.getElementById('{identifier}');
                        frameElement.src = "{path}";
                        
                        const thisButton = document.getElementById('{uniqueIDs[i]}{identifier}button');
                        thisButton.style.backgroundColor = '#BA2F00';
                        {othersToLight}
                    }}
                    </script>
        """
        
    
    # Add info button to go below toggling buttons
    plotlyExplanation=f"""
        The <b>new</b> interactive sound level graph offers many new useful features to explore annual and seasonal soundscape conditions. Hover over the grey shaded boxes and black vertical dash lines to see detailed information about the frequencies of interest, and hover over the colorful solid or black dotted median lines for details on the sound levels at each frequency bin by year, season, or overall median. Hover over the monitoring effort graph below to see the number of recording days for each month or season, as well as the threshold for the minimum number of days required for a month to be included in the annual plots. <br><br>

        Select or deselect the visibility of a year or season's sound levels, as well as the effort bars, by clicking on either plot’s legend. Single click on the legend's year or season that you would like to deselect, or 'turn off', or double click on a legend's year or season to view only that line, and ‘turn off’ all others. Deselect all seasonal and yearly median data lines to view only the overall median line, and then double click any year or season’s legend text to make all lines visible again. <br><br>
        
        The following icons, which you can find at the top right of the sound level and effort figures, let you change your mouse performance, axes display, and hover feature settings. <br><br>
        
        <img src='{inputDir}/zoom.png'></img> ‘Zoom’ is the default mouse selection, which lets you zoom into the data by drawing a box over the graph. <br>
        <img src='{inputDir}/pan.png'></img> 'Pan' is the other mouse performance option, which allows you to grab and drag the graph. <br>
        <img src='{inputDir}/zoomIn.png'></img> ‘Zoom in’. <br>
        <img src='{inputDir}/zoomOut.png'></img> ‘Zoom out’. <br>
        <img src='{inputDir}/autoscale.png'></img> ‘Autoscale’ scales the graph axes to what the interactive graph thinks is best. <br>
        <img src='{inputDir}/resetAxes.png'></img> ‘Reset axes’ sets the axes back to their original minimum and maximum values.<br>
        <img src='{inputDir}/showClosestDataOnHover.png'></img> 'Show closest data on hover' is the default hover mode, which only shows one hover feature at a time. <br>
        <img src='{inputDir}/compareDataOnHover.png'></img> 'Compare data on hover' shows more than one feature at a time. 
    """
    
    info_button = f'''
        <button style="padding: 10px; color: black; margin: 10px; background-color: white; width: 275px; height:  display: block;" onclick="document.getElementById(&#39;infoModal&#39;).showModal()">How to interact with this graphic</button>
            <dialog id="infoModal">
                <p>{plotlyExplanation}</p>
                <button style="padding: 10px; color: black; margin: 4px 0; background-color: white; width: 100px; display: block;" onclick="document.getElementById(&#39;infoModal&#39;).close()">
                    Close
                </button>
            </dialog>
    '''
    
    container_start = '<div style="display: flex; flex-direction: row; align-items: flex-start; gap: 20px;">'
    button_column = f'<div style="display: flex; flex-direction: column;">{buttons}</div>'
    container_end = '</div>'
    
    resize_script = """
    <script>
    function resizePlotlyIframe(frame) {
        setTimeout(function() {
            if (frame.contentWindow && frame.contentWindow.document) {
                var contentHeight = frame.contentWindow.document.documentElement.scrollHeight;
                var contentWidth = frame.contentWindow.document.documentElement.scrollWidth;
                frame.style.height = contentHeight + 'px'; 
                frame.style.width = contentWidth + 'px'; 
            }
        }, 300);
    }
    </script>
    """
    
    return info_button + container_start + button_column + initialIframe + container_end + scripts + resize_script
