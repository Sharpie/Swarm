<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:html="http://www.w3.org/TR/REC-html40"
 xmlns:xt="http://www.jclark.com/xt"
 version="1.0"
 exclude-result-prefixes="html"
 extension-element-prefixes="xt">
 
 <xsl:import href="xsl/website/xttabular.xsl"/>
 
 <xsl:output method="html" encoding="ISO-8859-1"/>
 
 <xsl:param name="toc.spacer.text">&#160;</xsl:param>
 
 <!-- customization variables -->
 <!--
 <xsl:variable name="bodybgcolor">#99CCFF</xsl:variable>
 <xsl:variable name="navbgcolor">#99CCFF</xsl:variable>
 -->
 <xsl:variable name="navtocwidth">175</xsl:variable>
 <xsl:variable name="bodybgcolor">#c0c0c0</xsl:variable>
 <xsl:variable name="navbgcolor">#c0c0c0</xsl:variable>
 
 <xsl:variable name="nav.table.summary" select="1"/>
 
 <xsl:variable name="footer.hr" select="1"/>
 
 <xsl:variable name="admon.graphics.path">
  <xsl:text>graphics/icons/</xsl:text>
 </xsl:variable>
 
 <xsl:variable name="admon.graphics" select="1"/>
 
 <xsl:template match="emphasis[@role='strong']">
    <strong><xsl:apply-templates/></strong>
  </xsl:template>

  <xsl:template match="sect1[@role='nongraphical.admonition']//note|sect1[@role='nongraphical.admonition']//important|sect1[@role='nongraphical.admonition']//warning|sect1[@role='nongraphical.admonition']//caution|sect1[@role='nongraphical.admonition']//tip">
    <xsl:choose>
      <xsl:when test="$admon.graphics != 0">
        <xsl:call-template name="graphical.admonition.homepage"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="nongraphical.admonition"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:variable name="admon.style.homepage">
    <xsl:text>margin-left: 0.25in; margin-right: 0.25in;</xsl:text>
  </xsl:variable>

  <xsl:template name="admon.graphic.homepage">
    <xsl:param name="node" select="."/>
    <xsl:value-of select="$admon.graphics.path"/>
    <xsl:choose>
      <xsl:when test="name($node)='note'">rarrow.png</xsl:when>
      <xsl:when test="name($node)='warning'">rarrow.png</xsl:when>
      <xsl:when test="name($node)='caution'">rarrow.png</xsl:when>
      <xsl:when test="name($node)='tip'">rarrow.png</xsl:when>
      <xsl:when test="name($node)='important'">rarrow.png</xsl:when>
      <xsl:otherwise>rarrow.png</xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="graphical.admonition.homepage">
    <div class="{name(.)}">
      <xsl:if test="$admon.style.homepage">
        <xsl:attribute name="style">
          <xsl:value-of select="$admon.style.homepage"/>
        </xsl:attribute>
      </xsl:if>
      <table border="0">
        <tr>
          <td rowspan="2" align="left" valign="top">
            <xsl:attribute name="width">
              <xsl:call-template name="admon.graphic.width"/>
            </xsl:attribute>
            <img>
              <xsl:attribute name="src">
                <xsl:call-template name="admon.graphic.homepage"/>
              </xsl:attribute>
            </img>
          </td>
          <td>
            <xsl:choose>
              <xsl:when test="./title">
                <xsl:apply-templates select="./title" 
                  mode="graphic.admonition.title.mode"/>

                  <!-- print out date in brackets, if present -->
                    <xsl:if test="@revision">
                      <font size="-1">
                        <em>
                          <xsl:text> (</xsl:text>
                          <xsl:value-of select="@revision"/>
                          <xsl:text>)</xsl:text>
                        </em>
	 </font>
	</xsl:if>
       </xsl:when>
                  <xsl:otherwise>
	<a>
	 <xsl:attribute name="name">
	  <xsl:call-template name="object.id"/>
	 </xsl:attribute>
	 <xsl:call-template name="gentext.element.name"/>
	</a>
       </xsl:otherwise>
      </xsl:choose>
     </td>
    </tr>
    <tr>
     <td colspan="2" align="left" valign="top">
      <xsl:apply-templates/>
     </td>
    </tr>
   </table>
  </div>
 </xsl:template>


 <xsl:template match="title" mode="graphic.admonition.title.mode">
  <xsl:variable name="id">
   <xsl:call-template name="object.id">
    <xsl:with-param name="object" select=".."/>
   </xsl:call-template>
  </xsl:variable>
  <b class="title">
   <a name="{$id}">
    <xsl:apply-templates/>
   </a>
  </b>
 </xsl:template>
 
 <xsl:variable name="toc.pointer.graphic" select="1"/>
 
 <xsl:variable name="toc.pointer.image">graphics/icons/rarrow.png</xsl:variable>
 
 <!-- home navigation elements -->
 <xsl:template name="home.navhead.upperright">
  <xsl:variable name="homebanner"
   select="/website/homepage/config[@param='homebanner']/@value"/>
  <xsl:variable name="homebanneralt"
   select="/website/homepage/config[@param='homebanner']/@altval"/>  
  <xsl:variable name="relpath">
   <xsl:call-template name="root-rel-path">
    <xsl:with-param name="webpage" select="."/>
   </xsl:call-template>
  </xsl:variable>
  
  <span class="homenavhead">
   <img src="{$relpath}{$homebanner}"
    alt="{$homebanneralt}" align="center" border="0"/>
  </span><p></p>
 </xsl:template>
 
 <!-- suppress navhead -->
 <xsl:template name="home.navhead">
  <xsl:text>&#160;</xsl:text>
 </xsl:template>
 
 <!-- suppress remarks (comments) -->
 <xsl:template match="remark"/>
 
 <!-- suppress generation of abstract and author in head -->
 <xsl:template match="head/abstract"/>
 
 <xsl:template match="head/author"/>
 
 <xsl:template match="head/style[@type='div']">
  <div>
   <xsl:attribute name="id">
    <xsl:value-of select="."/>
   </xsl:attribute>
  </div>
 </xsl:template>
    
 <!-- templates for which no matches are currently in XSL -->
 
 <xsl:template match="orgname">      
  <xsl:apply-templates/>
  </xsl:template>
 
 <xsl:template match="entry[@spanname='all']">      
  <em><xsl:apply-templates/></em><p></p>
  </xsl:template>
 
 <xsl:template match="application">
  <strong><span class="{name(.)}">
    <xsl:apply-templates/>
   </span></strong>
 </xsl:template>
 
 <!-- sort biblioentries in alpha order -->
 <xsl:template match="bibliography|bibliography/bibliodiv">
  <xsl:apply-templates select="*[not(self::biblioentry)]"/>
  <xsl:apply-templates select="biblioentry" mode="bibliography.mode">
   <xsl:sort data-type="text" select="./abbrev[1]"/>
  </xsl:apply-templates>
 </xsl:template>        
 
 <xsl:strip-space elements="citetitle"/>

 <xsl:template match="citetitle[@pubwork='article' or @pubwork='chapter']" mode="bibliography.mode">
  <xsl:text>"</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>"</xsl:text>
 </xsl:template>
 
 <xsl:template match="conftitle" mode="bibliography.mode">
  <em><xsl:apply-templates/></em>
 </xsl:template>
 
 <xsl:template match="biblioset[@relation='series']" mode="bibliography.mode">
  <br/><em>Series: </em> <xsl:apply-templates mode="bibliography.mode"/>
 </xsl:template>

 <!-- don't suppress abstracts, and indent them -->
  <xsl:template match="abstract" mode="bibliography.mode">
  <dl><dd><strong><em>Abstract: </em></strong>
    <xsl:for-each select="para">
     <xsl:apply-templates/><p></p>
    </xsl:for-each></dd>
  </dl>
 </xsl:template>
 
 <xsl:template match="affiliation">
  <xsl:apply-templates/>
 </xsl:template>
 
 <!-- these provide a layout for the SwarmFest 2001 program page -->
 <xsl:template match="citetitle" mode="bibliomixed.mode">
  <hr/>
  <em><xsl:apply-templates mode="bibliomixed.mode"/></em>
 </xsl:template>
 
 <xsl:template match="subtitle" mode="bibliomixed.mode">
     <em><xsl:apply-templates mode="bibliomixed.mode"/></em>
 </xsl:template>
 
 <xsl:template match="affiliation" mode="bibliomixed.mode">
  <xsl:for-each select="address/email">
   
   <xsl:text>&lt;</xsl:text>
   <a>
    <xsl:attribute name="href">mailto:<xsl:value-of select="."/></xsl:attribute>
    <xsl:apply-templates/>
   </a>
   <xsl:text>&gt;</xsl:text>
  </xsl:for-each>
  <br/>
  <xsl:for-each select="orgdiv">
   <xsl:apply-templates/><br></br>
  </xsl:for-each>
  <xsl:for-each select="orgname">
   <xsl:apply-templates/><br></br>
  </xsl:for-each>
 </xsl:template>
 
 <xsl:template match="firstname" mode="bibliomixed.mode">
  <br/>
  <xsl:apply-templates/>
  <xsl:text> </xsl:text>
 </xsl:template>
 
 <xsl:template match="surname" mode="bibliomixed.mode">
  <xsl:apply-templates/>
  <xsl:text> </xsl:text>
 </xsl:template>
 
 <!-- End of SwarmFest 2001 hacks -->
 
 <!-- use the url as text for a ulink if the content is empty -->
 <xsl:template match="ulink">
  <xsl:choose>
   <xsl:when test=".=''">
    <a>
     <xsl:attribute name="href">
      <xsl:value-of select="@url"/>
     </xsl:attribute>
     <xsl:value-of select="@url"/>
    </a>
   </xsl:when>
   <xsl:otherwise>
    <a>
     <xsl:attribute name="href">
      <xsl:value-of select="@url"/>
     </xsl:attribute>
     <xsl:apply-templates/>
    </a>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:template>
 
 <!-- rules for active-jobs role subclassing on project info pages -->
  <xsl:template name="generate-tip-image">
  <img>
   <xsl:attribute name="src">graphics/icons/tip.png</xsl:attribute>
   <xsl:attribute name="hspace">5</xsl:attribute>
   <xsl:attribute name="alt">Active jobs</xsl:attribute>
  </img>
 </xsl:template>
 
 <xsl:template match="firstterm">
  <xsl:if test="@role='active-jobs'">
   <xsl:call-template name="generate-tip-image"/>
  </xsl:if>
  <xsl:apply-templates/>
 </xsl:template>
 
 <xsl:template match="seglistitem">
  <hr></hr>
  <xsl:if test="@role='active-jobs'">
   <xsl:call-template name="generate-tip-image"/>
  </xsl:if>
  <xsl:apply-templates/>
 </xsl:template>
 
 <!-- rules for role subclassing on programmer info pages -->
 <xsl:template match="variablelist[@role='programmers']//corpauthor">
  <strong><em>
    <xsl:text>Group: </xsl:text>
   </em></strong>
  <xsl:apply-templates/><br></br>
 </xsl:template>
 
 <xsl:template match="variablelist[@role='programmers']//term">
  <xsl:apply-templates/><p></p>
  <em><strong>Experience using Swarm:</strong></em><p></p>
 </xsl:template>
 
 <xsl:template match="variablelist[@role='programmers']//varlistentry">
  <hr></hr><xsl:apply-templates/>
 </xsl:template>
 
  <xsl:template match="variablelist[@role='programmers']//affiliation">
  <xsl:choose>
   <xsl:when test="not(../../corpauthor)">
    <xsl:for-each select="address">
     <xsl:apply-templates/>
    </xsl:for-each>
    <br></br><em><strong>Affiliation(s): </strong></em>
    <xsl:for-each select="orgdiv">
     <xsl:apply-templates/><br></br>
    </xsl:for-each>
    <xsl:for-each select="orgname">
     <xsl:apply-templates/>
    </xsl:for-each>
   </xsl:when>
   <xsl:otherwise>
    <xsl:for-each select="orgdiv">
     <xsl:apply-templates/><br></br>
    </xsl:for-each>
    <xsl:for-each select="orgname">
     <xsl:apply-templates/><br></br>
    </xsl:for-each>
    <xsl:for-each select="address">
     <xsl:apply-templates/>
    </xsl:for-each>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:template>
 
 <xsl:template match="variablelist[@role='programmers']//phone">
  <em>ph: </em><xsl:apply-templates/><br></br>
 </xsl:template>
 
 <xsl:template match="variablelist[@role='programmers']//fax">
      <em>fax: </em><xsl:apply-templates/><br></br>
 </xsl:template>
 
 <xsl:template match="variablelist[@role='programmers']//author">
  <em><strong>Contact: </strong></em>
  <xsl:apply-templates/> 
  <br></br>
 </xsl:template>
 
 
 <xsl:template match="variablelist[@role='programmers']//firstname">
  <xsl:apply-templates/>
  <xsl:text> </xsl:text>
 </xsl:template>
 
 <xsl:template match="variablelist[@role='programmers']//surname">
  <xsl:apply-templates/>
  <xsl:if test="../affiliation/jobtitle">
   <xsl:text>, </xsl:text>        
   <xsl:value-of select="../affiliation/jobtitle"/>
  </xsl:if>
    </xsl:template>
 
 <xsl:param name="use-id-html-filename" select="1"/>
 
 <!-- override standard filename generation and use id as html
 filename -->
 <xsl:template match="homepage|webpage" mode="filename">
  <xsl:variable name="dir">
   <xsl:choose>
    <xsl:when test="config[@param='dir']/@value">
     <xsl:value-of select="config[@param='dir']/@value"/>
     <xsl:text>/</xsl:text>
    </xsl:when>
   </xsl:choose>
  </xsl:variable>
  
  <xsl:variable name="fname">
   <xsl:choose>
    <xsl:when test="$use-id-html-filename = '1'">
     <xsl:choose>                
      <xsl:when test="@id">
       <xsl:value-of select="@id"/><xsl:text>.html</xsl:text>
      </xsl:when>
      <xsl:otherwise>
       <xsl:message>
	<xsl:text>Error no ID for webpage: </xsl:text>
	<xsl:text>can't generate HTML filename</xsl:text>
       </xsl:message>
      </xsl:otherwise>
     </xsl:choose>
    </xsl:when>
    <xsl:otherwise>
     <xsl:choose>
      <xsl:when test="config[@param='filename']/@value">
       <xsl:value-of select="config[@param='filename']/@value"/>
      </xsl:when>
      <xsl:otherwise>index.html</xsl:otherwise>
     </xsl:choose>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:variable>
  
  <xsl:value-of select="concat($dir, $fname)"/>
 </xsl:template>
 
 <!-- don't generate homebanner in nav.toc on homepage -->
 <xsl:template name="nav.toc">
  <xsl:param name="curpage" select="."/>
  <xsl:param name="pages"></xsl:param>
  
  <xsl:variable name="homebanner"
   select="/website/homepage/config[@param='homebanner']/@value"/>
  <xsl:variable name="homebanneralt"
   select="/website/homepage/config[@param='homebanner']/@altval"/>
  <xsl:variable name="banner"
   select="/website/homepage/config[@param='banner']/@value"/>
  <xsl:variable name="banneralt"
   select="/website/homepage/config[@param='banner']/@altval"/>
  
  <xsl:variable name="hometitle" select="/website/homepage/head/title[1]"/>
  
  <xsl:variable name="relpath">
   <xsl:call-template name="root-rel-path">
    <xsl:with-param name="webpage" select="$curpage"/>
   </xsl:call-template>
  </xsl:variable>
  
  <xsl:choose>
   <xsl:when test="/website/homepage=$curpage">
    <!-- don't generate any homebanner -->
   </xsl:when>
   <xsl:otherwise>
    <a>
     <xsl:attribute name="href">
      <xsl:call-template name="href.target">
       <xsl:with-param name="object" select="/website/homepage"/>
      </xsl:call-template>
     </xsl:attribute>
     <img src="{$relpath}{$banner}"
      alt="{$banneralt}" align="left" border="0"/>
    </a>
    <br clear="all"/>
    <br/>
   </xsl:otherwise>
  </xsl:choose>
  
  <xsl:apply-templates select="$pages" mode="table.toc">
   <xsl:with-param name="curpage" select="$curpage"/>
  </xsl:apply-templates>
  <br/>
 </xsl:template>
 
 <xsl:template match="webpage" mode="table.toc">
  <xsl:param name="curpage" select="."/>
  <xsl:param name="toclevel" select="1"/>
  
  <xsl:variable name="notoc" select="config[@param='notoc']/@value"/>
  
  <xsl:choose>
   <xsl:when test="$notoc='1'">
    <!--
    <xsl:message>
    <xsl:text>Not in TOC: </xsl:text>
    <xsl:value-of select="head/title"/>
   </xsl:message>
-->
   </xsl:when>
   <xsl:otherwise>
    <xsl:call-template name="webpage.nav.toc">
     <xsl:with-param name="curpage" select="$curpage"/>
     <xsl:with-param name="toclevel" select="$toclevel"/>
    </xsl:call-template>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:template>
 

 <!-- overwrite from stock stylesheet to do -->
 <!-- something different on homepage -->
 <xsl:template name="webpage.nav.toc">
  <xsl:param name="curpage" select="."/>
  <xsl:param name="toclevel" select="1"/>
  <xsl:variable name="id">
   <xsl:call-template name="object.id"/>
  </xsl:variable>
  
  <xsl:variable name="title" select="(head/title|head/titleabbrev)[last()]"/>
  
  <xsl:variable name="summary" select="head/summary[1]"/>
  
  <xsl:variable name="abstract" select="head/abstract/para"/>
  
  <xsl:variable name="ishomepage">
   <xsl:choose>
    <xsl:when test="//website/homepage=$curpage">1</xsl:when>
    <xsl:otherwise>0</xsl:otherwise>
   </xsl:choose>
  </xsl:variable>
  
  <xsl:variable name="relpath">
   <xsl:call-template name="root-rel-path">
    <xsl:with-param name="webpage" select="$curpage"/>
   </xsl:call-template>
  </xsl:variable>
  
  <xsl:call-template name="insert.spacers">
   <xsl:with-param name="count" select="$toclevel"/>
   <xsl:with-param name="relpath" select="$relpath"/>
  </xsl:call-template>
    
  <font>
   <xsl:if test="$toclevel>1">
    <xsl:attribute name="size">
     <xsl:text>-</xsl:text>
     <xsl:value-of select="$toclevel - 1"/>
    </xsl:attribute>
   </xsl:if>
   
   <xsl:choose>
    <xsl:when test=".=$curpage">
     <span class="navtoc">
      <xsl:choose>
       <xsl:when test="$toc.pointer.graphic != 0">
	<img src="{$relpath}{$toc.pointer.image}"
	 alt="{$toc.pointer.text}"/>
       </xsl:when>
       <xsl:otherwise>
	<xsl:value-of select="$toc.pointer.text"/>
       </xsl:otherwise>
      </xsl:choose>
      <xsl:apply-templates select="$title" mode="table.toc"/>
     </span>
     <br/>
    </xsl:when>
    <xsl:otherwise>
     <span class="navtoc">
      <xsl:call-template name="insert.spacers">
       <xsl:with-param name="count" select="1"/>
       <xsl:with-param name="relpath" select="$relpath"/>
      </xsl:call-template>
      <a>
       <xsl:attribute name="href">
	<xsl:call-template name="href.target">
	 <xsl:with-param name="from-page" select="$curpage"/>
	</xsl:call-template>
       </xsl:attribute>
       <xsl:if test="$ishomepage='1'">
	<xsl:attribute name="title">
	 <xsl:value-of select="normalize-space($abstract)"/>
	</xsl:attribute>
	
	<!--<xsl:attribute name="class">
	<xsl:text>bannerLink</xsl:text>
       </xsl:attribute> -->
       </xsl:if> 
       <xsl:apply-templates select="$title" mode="table.toc"/>
      </a>
     </span>
     <br/>
    </xsl:otherwise>
   </xsl:choose>
  </font>
  
  <xsl:variable name="isancestor">
   <xsl:call-template name="isancestor">
    <xsl:with-param name="curpage" select="$curpage"/>
    <xsl:with-param name="here" select="."/>
   </xsl:call-template>
  </xsl:variable>
  
  <xsl:choose>
   <!-- new part: if current page is the homepage: -->
   <!-- generate a two-deep toc for every webpage -->
   <xsl:when test="$ishomepage='1'">
    <!-- <xsl:when test="//website/homepage=$curpage"> -->
    <xsl:if test="$toclevel &lt; 2">
     <xsl:apply-templates select="webpage" mode="table.toc">
      <xsl:with-param name="curpage" select="$curpage"/>
      <xsl:with-param name="toclevel" select="$toclevel + 1"/>
     </xsl:apply-templates>
    </xsl:if>
   </xsl:when>
   <!-- otherwise generate the default -->
   <xsl:otherwise>
    <xsl:if test="$isancestor='1'"> 
     <xsl:apply-templates select="webpage" mode="table.toc">
      <xsl:with-param name="curpage" select="$curpage"/>
      <xsl:with-param name="toclevel" select="$toclevel + 1"/>
     </xsl:apply-templates>
    </xsl:if>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:template>
 
 <!-- replace all occurences of the character(s) `from'
 by the string `to' in the string `string'.-->
 <xsl:template name="string-replace" >
  <xsl:param name="string"/>
  <xsl:param name="from"/>
  <xsl:param name="to"/>
  <xsl:choose>
   <xsl:when test="contains($string,$from)">
    <xsl:value-of select="substring-before($string,$from)"/>
    <xsl:value-of select="$to"/>
    <xsl:call-template name="string-replace">
     <xsl:with-param name="string" 
      select="substring-after($string,$from)"/>
     <xsl:with-param name="from" select="$from"/>
     <xsl:with-param name="to" select="$to"/>
    </xsl:call-template>
   </xsl:when>
   <xsl:otherwise>
    <xsl:value-of select="$string"/>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:template>
 
</xsl:stylesheet>

<!-- DSSSL options yet to be translated to XSL

(define %shade-verbatim%
    ;; Should verbatim environments be shaded?
    #t) 
-->


<!-- 
Local variables:
mode: xml
sgml-default-dtd-file: "org.ced"
sgml-indent-step: 1
sgml-indent-data: 1
End:
-->

	