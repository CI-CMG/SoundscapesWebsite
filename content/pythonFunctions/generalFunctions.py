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
        buttons += f'<button id="{site}{identifier}button" onclick="{site}{identifier}()" style="padding: 10px; color: white; margin: 4px 0; background-color: {initialColor}; text-transform: uppercase; width: 150px; display: block;">{site}</button>'

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
    initialImage = f'<img alt="{altText}" src="{path}" width="{width}" id="{identifier}" onclick="this.requestFullscreen()" style="display: block; margin-left: auto; margin-right: auto">'
    return initialImage
    
def addPlotly(sourceHTML):
	return f"""
            <iframe
                src="resources/{sourceHTML}"
                name="targetframe"
                allowTransparency="true"
                scrolling="no"
                frameborder="0"
                width="700px"
                height="850px"
            >
            </iframe>
			"""
  
def embedMapViewer(srcLink):
    return f'<embed src="{srcLink}" style="width:900px; height: 800px;">'
