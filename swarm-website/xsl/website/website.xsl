<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:html="http://www.w3.org/TR/REC-html40"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                version='1.0'>

<!-- ********************************************************************
     $Id: website.xsl,v 1.1.1.1 2003-05-16 20:00:37 mgd Exp $
     ********************************************************************

     This file is part of the XSL DocBook Stylesheet distribution.
     See ../README or http://nwalsh.com/docbook/xsl/ for copyright
     and other information.

     ******************************************************************** -->

<!-- ==================================================================== -->

<xsl:include href="http://docbook.sourceforge.net/release/xsl/snapshot/xhtml/docbook.xsl"/>
<!-- <xsl:include href="../docbook/xhtml/docbook.xsl"/> -->

<xsl:include href="xbel.xsl"/>

<xsl:strip-space elements="website homepage webpage"/>

<xsl:output method="xml"/>

<!-- ==================================================================== -->

<xsl:variable name="VERSION">1.0</xsl:variable>

<doc:variable name="VERSION" xmlns="">
<refpurpose>Stylesheet version</refpurpose>
<refdescription>
<para>Identifies the stylesheet version. The version number is incremented
whenever the stylesheet is republished with user-visible changes.</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->

<xsl:variable name="using.chunker" select="true()"/>

<doc:variable name="using.chunker" xmlns="">
<refpurpose>Will the output be chunked?</refpurpose>
<refdescription>
<para>In addition to providing chunking, the chunker can cleanup a
number of XML to HTML issues. If the chunker is not being used, the
stylesheets try to avoid producing results that will not appear properly
in browsers.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->

<xsl:variable name="html.stylesheet"></xsl:variable>

<doc:variable name="html.stylesheet" xmlns="">
<refpurpose>Name of the stylesheet to use in the generated HTML</refpurpose>
<refdescription>
<para>The name of the stylesheet to place in the HTML <sgmltag>LINK</sgmltag>
tag, or the empty string to suppress the stylesheet <sgmltag>LINK</sgmltag>.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->

<xsl:variable name="html.stylesheet.type"></xsl:variable>

<doc:variable name="html.stylesheet.type" xmlns="">
<refpurpose>The type of the stylesheet used in the generated HTML</refpurpose>
<refdescription>
<para>The type of the stylesheet to place in the HTML <sgmltag>link</sgmltag> tag.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->

<xsl:variable name="footer.hr" select="1"/>

<doc:variable name="footer.hr" xmlns="">
<refpurpose>Toggle &lt;HR> before footer</refpurpose>
<refdescription>
<para>If non-zero, an &lt;HR> is generated at the bottom of each web page,
before the footer.</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->

<xsl:variable name="feedback.href"></xsl:variable>

<doc:variable name="feedback.href" xmlns="">
<refpurpose>HREF for feedback link</refpurpose>
<refdescription>
<para>The <varname>feedback.href</varname> value is used as the value
for the <sgmltag class="attribute">href</sgmltag> attribute on the feedback
link, if it is not the empty string. If <varname>feedback.href</varname>
is empty, no feedback link is generated.</para>
</refdescription>
</doc:variable>

<xsl:variable name="feedback.title" select="0"/>

<doc:variable name="feedback.title" xmlns="">
<refpurpose>Toggle use of titles in feedback</refpurpose>
<refdescription>
<para>If <varname>feedback.title</varname> is non-zero, the title of the
current page will be added to the feedback link. This can be used, for
example, if the <varname>feedback.href</varname> is a CGI script.</para>
</refdescription>
</doc:variable>

<xsl:variable name="feedback.link.text">Feedback</xsl:variable>

<doc:variable name="feedback.link.text" xmlns="">
<refpurpose>The text of the feedback link</refpurpose>
<refdescription>
<para>The contents of this variable is used as the text of the feedback
link if <varname>feedback.href</varname> is not empty. If
<varname>feedback.href</varname> is empty, no feedback link is
generated.</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->

<xsl:variable name="admon.graphics" select="1"/>

<doc:variable name="admon.graphics" xmlns="">
<refpurpose>Use graphics in admonitions?</refpurpose>
<refdescription>
<para>If true (non-zero), admonitions are presented in an alternate style
that uses a graphic.  Default graphics are provided in the distribution.
</para>
</refdescription>
</doc:variable>

<xsl:variable name="admon.graphics.path">
  <xsl:text>graphics/admon/</xsl:text>
</xsl:variable>

<doc:variable name="admon.graphics.path" xmlns="">
<refpurpose>Path to admonition graphics</refpurpose>
<refdescription>
<para>Sets the path, probably relative to the directory where the HTML
files are created, to the admonition graphics.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->

<doc:template name="admon.graphic">
<refpurpose>Select appropriate admonition graphic</refpurpose>
<refdescription>
<para>Selects the appropriate admonition graphic file and returns the
fully qualified path to it.</para>
</refdescription>
<refparam>
<variablelist>
<varlistentry><term>node</term>
<listitem>
<para>The source node to use for the purpose of selection. It should
be one of the admonition elements (<sgmltag>note</sgmltag>,
<sgmltag>warning</sgmltag>, etc.). The default node is the context
node.</para>
</listitem>
</varlistentry>
</variablelist>
</refparam>
<refreturns>
<para>The fully qualified path to the admonition graphic. If the
<varname>node</varname> is not an admonition element, the
  <quote>note</quote> graphic is returned.</para>
</refreturns>
</doc:template>

<xsl:template name="admon.graphic">
  <xsl:param name="node" select="."/>
  <xsl:call-template name="root-rel-path"/>
  <xsl:value-of select="$admon.graphics.path"/>
  <xsl:choose>
    <xsl:when test="name($node)='note'">note.png</xsl:when>
    <xsl:when test="name($node)='warning'">warning.png</xsl:when>
    <xsl:when test="name($node)='caution'">caution.png</xsl:when>
    <xsl:when test="name($node)='tip'">tip.png</xsl:when>
    <xsl:when test="name($node)='important'">important.png</xsl:when>
    <xsl:otherwise>note.png</xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="/">
  <xsl:variable name="title" select="/website/homepage/head/title"/>
  <html>
    <head>
      <title><xsl:value-of select="$title"/></title>
      <xsl:if test="$html.stylesheet">
	<link rel="stylesheet" href="{$html.stylesheet}"
	  type="{$html.stylesheet.type}"/>
      </xsl:if>
      <meta name="generator" content="Website XSL Stylesheet V{$VERSION}"/>
    </head>
    <body>
      <xsl:apply-templates/>
    </body>
  </html>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="website">
  <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template name="home.navhead">
  <div class="navhomehead">
    <div class="navbar">
      <xsl:text>Home</xsl:text>
      <xsl:text>@</xsl:text>
      <xsl:call-template name="webpage.header">
        <xsl:with-param name="thispage" select="."/>
        <xsl:with-param name="pages" select="/website/webpage"/>
        <xsl:with-param name="text.before"> | </xsl:with-param>
      </xsl:call-template>
      <hr/>
    </div>
  </div>
</xsl:template>

<xsl:template match="homepage">
  <xsl:variable name="id">
    <xsl:call-template name="object.id"/>
  </xsl:variable>

  <div id="{$id}" class="{name(.)}">
    <a name="{$id}"/>

    <xsl:apply-templates select="head" mode="head.mode"/>
    <xsl:apply-templates select="config" mode="head.mode"/>

    <xsl:call-template name="home.navhead"/>

    <xsl:apply-templates select="./head/title" mode="title.mode"/>

    <xsl:apply-templates/>

    <xsl:call-template name="process.footnotes"/>

    <xsl:call-template name="webpage.footer"/>
  </div>
</xsl:template>

<xsl:template name="page.navhead">
  <div class="navhead">
    <div class="navbar">
      <a>
        <xsl:attribute name="href">
          <xsl:call-template name="href.target">
            <xsl:with-param name="object" select="/website/homepage"/>
          </xsl:call-template>
        </xsl:attribute>
        <xsl:text>Home</xsl:text>
      </a>
      <xsl:call-template name="webpage.header">
        <xsl:with-param name="thispage" select="."/>
        <xsl:with-param name="pages" select="/website/webpage"/>
        <xsl:with-param name="text.before"> | </xsl:with-param>
      </xsl:call-template>
    </div>

    <div class="navbar">
      <xsl:if test="ancestor::webpage">
        <xsl:call-template name="webpage.subheaders">
          <xsl:with-param name="pages" select="ancestor::webpage"/>
        </xsl:call-template>

        <xsl:apply-templates select="./head/title[1]" mode="toc.mode"/>
        <xsl:text>@</xsl:text>
      </xsl:if>

      <hr/>
    </div>
  </div>
</xsl:template>

<xsl:template match="webpage">
  <xsl:variable name="id"><xsl:call-template name="object.id"/></xsl:variable>

  <div id="{$id}" class="{name(.)}">
    <a name="{$id}"/>

    <xsl:apply-templates select="head" mode="head.mode"/>
    <xsl:apply-templates select="config" mode="head.mode"/>

    <xsl:call-template name="page.navhead"/>

    <xsl:apply-templates select="./head/title" mode="title.mode"/>

    <xsl:apply-templates/>

    <xsl:call-template name="process.footnotes"/>

    <xsl:call-template name="webpage.footer"/>
  </div>
</xsl:template>

<xsl:template name="webpage.subheaders">
  <xsl:param name="pages"></xsl:param>
  <xsl:param name="count" select="count($pages)"/>

  <xsl:call-template name="webpage.header">
    <xsl:with-param name="thispage" select="pages[$count=position()]"/>
    <xsl:with-param name="pages" select="$pages"/>
    <xsl:with-param name="text.after"> - </xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="webpage.header">
  <xsl:param name="thispage" select="."/>
  <xsl:param name="pages"></xsl:param>
  <xsl:param name="text.before"></xsl:param>
  <xsl:param name="text.after"></xsl:param>
  <xsl:param name="count" select="1"/>
  <xsl:choose>
    <xsl:when test="$count>count($pages)"></xsl:when>
    <xsl:otherwise>
      <xsl:variable name="curpage" select="$pages[$count=position()]"/>
      <xsl:variable name="title">
        <xsl:apply-templates select="$curpage/head/title[1]" mode="toc.mode"/>
      </xsl:variable>

      <xsl:value-of select="$text.before"/>
      <xsl:choose>
        <xsl:when test="$thispage=$curpage">
          <xsl:copy-of select="$title"/>
          <xsl:text>@</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <a>
            <xsl:attribute name="href">
              <xsl:call-template name="href.target">
                <xsl:with-param name="object" select="$curpage"/>
              </xsl:call-template>
            </xsl:attribute>
            <xsl:copy-of select="$title"/>
          </a>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:value-of select="$text.after"/>
      <xsl:call-template name="webpage.header">
        <xsl:with-param name="thispage" select="$thispage"/>
        <xsl:with-param name="pages" select="$pages"/>
        <xsl:with-param name="text.before" select="$text.before"/>
        <xsl:with-param name="text.after" select="$text.after"/>
        <xsl:with-param name="count" select="$count+1"/>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- ============================================================ -->

<xsl:template name="webpage.footer">
  <xsl:variable name="page" select="."/>
  <xsl:variable name="rcsdate" select="$page/config[@param='rcsdate']/@value"/>
  <xsl:variable name="title">
    <xsl:value-of select="$page/head/title[1]"/>
  </xsl:variable>
  <xsl:variable name="footers" select="$page/config[@param='footer']"/>
  <xsl:variable name="copyright" select="/website/homepage/head/copyright[1]"/>

  <div class="navfoot">
    <xsl:if test="$footer.hr != 0"><hr/></xsl:if>
    <table width="100%" border="0" summary="Footer navigation">
    <tr>
      <td width="33%" align="left">
        <span class="footdate"><xsl:value-of select="$rcsdate"/></span>
      </td>
      <td width="34%" align="center">
        <xsl:variable name="id">
          <xsl:call-template name="object.id">
            <xsl:with-param name="object" select="/website/homepage"/>
          </xsl:call-template>
        </xsl:variable>
        <span class="foothome">
          <a>
            <xsl:attribute name="href">
              <xsl:call-template name="href.target">
                <xsl:with-param name="object" select="/website/homepage"/>
              </xsl:call-template>
            </xsl:attribute>
            <xsl:text>Home</xsl:text>
          </a>
        </span>

        <xsl:apply-templates select="$footers" mode="footer.link.mode"/>
      </td>
      <td width="33%" align="right">
        <span class="footfeed">
          <xsl:if test="$feedback.href != ''">
            <a>
              <xsl:choose>
                <xsl:when test="$feedback.title != 0">
                  <xsl:attribute name="href">
                    <xsl:value-of select="$feedback.href"/>
                    <xsl:value-of select="$title"/>
                  </xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:attribute name="href">
                    <xsl:value-of select="$feedback.href"/>
                  </xsl:attribute>
                </xsl:otherwise>
              </xsl:choose>
	      <xsl:value-of select="$feedback.link.text"/>
	    </a>
          </xsl:if>
        </span>
      </td>
    </tr>
    <tr>
      <td colspan="3" align="right">
        <span class="footcopy">
          <xsl:apply-templates select="$copyright" mode="footer.mode"/>
        </span>
      </td>
    </tr>
    </table>
  </div>
</xsl:template>

<xsl:template match="config" mode="footer.link.mode">
  <span class="foothome">
    <xsl:text> | </xsl:text>
    <a href="{@value}"><xsl:value-of select="@altval"/></a>
  </span>
</xsl:template>

<!-- ============================================================ -->

<xsl:template match="copyright" mode="footer.mode">
  <span class="{name(.)}">
    <xsl:call-template name="gentext.element.name"/>
    <xsl:call-template name="gentext.space"/>
    <xsl:call-template name="dingbat">
      <xsl:with-param name="dingbat">copyright</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="gentext.space"/>
    <xsl:apply-templates select="year" mode="footer.mode"/>
    <xsl:call-template name="gentext.space"/>
    <xsl:apply-templates select="holder" mode="footer.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
  </span>
</xsl:template>

<xsl:template match="year" mode="footer.mode">
  <xsl:apply-templates/><xsl:text>, </xsl:text>
</xsl:template>

<xsl:template match="year[position()=last()]" mode="footer.mode">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="holder" mode="footer.mode">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="holder[@role]" mode="footer.mode">
  <a href="{@role}">
    <xsl:apply-templates/>
  </a>
</xsl:template>

<!-- ============================================================ -->

<xsl:template match="config">
</xsl:template>

<xsl:template match="config[@param='filename']" mode="head.mode">
  <xsl:variable name="dir" select="../config[@param='dir']"/>
  <xsl:choose>
    <xsl:when test="$dir">
      <xsl:processing-instruction name="dbhtml">
        <xsl:text>filename="</xsl:text>
        <xsl:value-of select="$dir/@value"/>
        <xsl:text>/</xsl:text>
        <xsl:value-of select="@value"/>
        <xsl:text>"</xsl:text>
      </xsl:processing-instruction>
    </xsl:when>
    <xsl:otherwise>
      <xsl:processing-instruction name="dbhtml">filename="<xsl:value-of select="@value"/>"</xsl:processing-instruction>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="head">
</xsl:template>

<xsl:template match="head" mode="head.mode">
  <xsl:variable name="nodes" select="*"/>
  <xsl:variable name="global"
                select="/website/homepage/head/*[@class='global']"/>
  <head>
    <xsl:apply-templates select="$nodes"/>
    <xsl:if test="not(.=/website/homepage/head)">
      <xsl:call-template name="process.globals">
        <xsl:with-param name="nodelist" select="$global"/>
      </xsl:call-template>
    </xsl:if>
    <!-- this is potentially slow -->
    <xsl:if test="..//xlink[@role='dynamic']">
      <script src="dynxbel.js" language="JavaScript"/>
    </xsl:if>
  </head>
</xsl:template>

<xsl:template name="process.globals">
  <xsl:param name="nodelist"></xsl:param>
  <xsl:if test="count($nodelist)>0">
    <xsl:variable name="node" select="$nodelist[1]"/>
    <xsl:variable name="rest" select="$nodelist[position()>1]"/>
    <xsl:choose>
      <xsl:when test="$node/@src">
        <xsl:variable name="relpath">
          <xsl:call-template name="root-rel-path"/>
        </xsl:variable>
        <link rel="stylesheet" href="{$relpath}{$node/@src}" type="text/css"/>
      </xsl:when>
      <xsl:otherwise>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:call-template name="process.globals">
      <xsl:with-param name="nodelist" select="$rest"/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template match="head/title">
  <title><xsl:apply-templates/></title>
</xsl:template>

<xsl:template match="head/title" mode="title.mode">
  <h1><xsl:apply-templates/></h1>
</xsl:template>

<xsl:template match="head/subtitle">
</xsl:template>

<xsl:template match="head/summary">
</xsl:template>

<xsl:template match="head/keywords">
  <link name="keywords" content="{.}"/>
</xsl:template>

<xsl:template match="head/copyright">
</xsl:template>

<xsl:template match="head/style">
</xsl:template>

<xsl:template name="directory-depth">
  <xsl:param name="dir"></xsl:param>
  <xsl:param name="count" select="0"/>

  <xsl:choose>
    <xsl:when test='contains($dir,"/")'>
      <xsl:call-template name="directory-depth">
        <xsl:with-param name="dir" select="substring-after($dir,'/')"/>
        <xsl:with-param name="count" select="$count + 1"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:choose>
        <xsl:when test='$dir=""'>
          <xsl:value-of select="$count"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$count + 1"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="root-rel-path">
  <xsl:param name="webpage" select="ancestor-or-self::webpage"/>
  <xsl:variable name="dir" select="$webpage[last()]/config[@param='dir']"/>
  <xsl:variable name="depth">
    <xsl:call-template name="directory-depth">
      <xsl:with-param name="dir" select="$dir/@value"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:choose>
    <xsl:when test="$dir">
      <xsl:call-template name="copy-string">
        <xsl:with-param name="string">../</xsl:with-param>
        <xsl:with-param name="count" select="$depth"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise></xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="head/style[@src]">
  <xsl:variable name="relpath"><xsl:call-template name="root-rel-path"/></xsl:variable>
  <link rel="stylesheet" href="{$relpath}{@src}" type="text/css"/>
</xsl:template>

<xsl:template match="head/script[@src]">
  <xsl:variable name="relpath">
    <xsl:call-template name="root-rel-path"/>
  </xsl:variable>
  <xsl:variable name="language">
    <xsl:choose>
      <xsl:when test="@language"><xsl:value-of select="@language"/></xsl:when>
      <xsl:otherwise>JavaScript</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <script src="{$relpath}{@src}" language="{$language}"></script>
</xsl:template>

<xsl:template match="webtoc">
  <xsl:choose>
    <xsl:when test="ancestor::homepage">
      <xsl:call-template name="toc">
        <xsl:with-param name="pages" select="/website/webpage"/>
        <xsl:with-param name="from-page"
                        select="(ancestor-or-self::webpage
                                 |ancestor-or-self::homepage)[last()]"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="toc">
        <xsl:with-param name="pages" select="ancestor::webpage[1]/webpage"/>
        <xsl:with-param name="from-page"
                        select="(ancestor-or-self::webpage
                                 |ancestor-or-self::homepage)[last()]"/>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="toc">
  <xsl:param name="pages"></xsl:param>
  <xsl:param name="from-page"></xsl:param>
  <ul>
    <xsl:apply-templates select="$pages" mode="toc.mode">
      <xsl:with-param name="from-page" select="$from-page"/>
    </xsl:apply-templates>
  </ul>
</xsl:template>

<xsl:template match="webpage" mode="toc.mode">
  <xsl:variable name="notoc" select="config[@param='notoc']/@value"/>
  <xsl:param name="from-page"></xsl:param>

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
      <xsl:variable name="title" select="./head/title[1]"/>
      <xsl:variable name="summary" select="./head/summary[1]"/>
      <li>
        <a>
          <xsl:attribute name="href">
            <xsl:call-template name="href.target">
              <xsl:with-param name="from-page" select="$from-page"/>
            </xsl:call-template>
          </xsl:attribute>
          <xsl:apply-templates select="$title" mode="toc.mode"/>
        </a>
        <xsl:if test="$summary">
          <xsl:text> - </xsl:text>
          <xsl:apply-templates select="$summary" mode="toc.mod"/>
        </xsl:if>
      </li>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="title" mode="toc.mode">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="summary" mode="toc.mode">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="titleabbrev">
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="footnote" mode="footnote.number">
  <xsl:choose>
    <xsl:when test="ancestor::table|ancestor::informaltable">
      <xsl:number level="any" from="table|informaltable" format="a"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:number level="any" from="webpage|homepage" format="1"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="process.footnotes">
  <!-- we're only interested in footnotes that occur on this page, not
       on descendants of this page (which will be similarly processed) -->
  <xsl:variable name="thispage"
                select="(ancestor-or-self::webpage
                         |ancestor-or-self::homepage)[last()]"/>
  <xsl:variable name="footnotes"
                select=".//footnote[(ancestor-or-self::webpage
                                     |ancestor-or-self::homepage)[last()]
                                    =$thispage]"/>
  <xsl:variable name="table.footnotes"
                select=".//table//footnote[(ancestor-or-self::webpage
                                     |ancestor-or-self::homepage)[last()]
                                    =$thispage]
                        |.//informaltable//footnote[(ancestor-or-self::webpage
                                     |ancestor-or-self::homepage)[last()]
                                    =$thispage]"/>

  <!-- Only bother to do this if there's at least one non-table footnote -->
  <xsl:if test="count($footnotes)>count($table.footnotes)">
    <div class="footnotes">
      <hr width="100" align="left"/>
      <xsl:apply-templates select="$footnotes" mode="process.footnote.mode"/>
    </div>
  </xsl:if>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="xlink">
  <xsl:choose>
    <xsl:when test="@actuate='auto'">
      <xsl:choose>
	<xsl:when test="@role='dynamic'">
	  <xsl:apply-templates select="document(@href)" mode="dynamic"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates select="document(@href)"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:otherwise>
      <xsl:choose>
        <xsl:when test="@role='olink'">
          <a href="/cgi-bin/olink?{@href}"><xsl:apply-templates/></a>
        </xsl:when>
        <xsl:otherwise>
          <a href="{@href}"><xsl:apply-templates/></a>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="olink">
  <xsl:text disable-output-escaping="yes">&lt;/p&gt;</xsl:text>
  <xsl:apply-templates select="document(unparsed-entity-uri(@targetdocent))"/>
  <xsl:text disable-output-escaping="yes">&lt;p&gt;</xsl:text>
</xsl:template>

<xsl:template match="html:*">
  <xsl:element name="{name(.)}" namespace="">
    <xsl:apply-templates select="@*" mode="copy"/>
    <xsl:apply-templates/>
  </xsl:element>
</xsl:template>

<xsl:template match="@*" mode="copy">
  <xsl:attribute name="{name(.)}">
    <xsl:value-of select="."/>
  </xsl:attribute>
</xsl:template>

<!-- ==================================================================== -->

</xsl:stylesheet>
