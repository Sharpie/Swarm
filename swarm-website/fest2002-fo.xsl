<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:html="http://www.w3.org/TR/REC-html40"
 xmlns:fo="http://www.w3.org/1999/XSL/Format"
 version="1.0">

 <xsl:import href="http://docbook.sourceforge.net/release/xsl/1.50.0/fo/docbook.xsl"/>

 <xsl:import href="fest2002-common.xsl"/>

 <xsl:param name="page.margin.inner">0.4in</xsl:param>
 <xsl:param name="page.margin.outer">0.4in</xsl:param>
 <xsl:param name="region.after.extent">0.25in</xsl:param>
 <xsl:param name="region.before.extent">0.25in</xsl:param>
 <xsl:param name="body.margin.bottom">0.35in</xsl:param>
 <xsl:param name="body.margin.top">0.35in</xsl:param>
 <xsl:param name="title.margin.left" select="'-3pc'"/>

 <xsl:param name="title.font.family">Helvetica</xsl:param>
 <xsl:param name="body.font.family">Helvetica</xsl:param>

 <xsl:template match="emphasis[@role='strong']">
  <xsl:call-template name="inline.boldseq"/>
 </xsl:template>

 <xsl:template match="para/emphasis">
  <xsl:call-template name="inline.italicseq"/>
 </xsl:template>

 <!-- don't output elements marked for online only -->
 <xsl:template match="//*[@role='online-only']"/>
 
 <!-- sort biblioentries in alpha order -->
 <xsl:template match="bibliography|bibliography/bibliodiv">
  <xsl:call-template name="sort-biblio-common"/>
 </xsl:template>        

 <xsl:template match="bibliomixed">
  <xsl:variable name="id">
   <xsl:call-template name="object.id"/>
  </xsl:variable>

  <fo:block id="{$id}" xsl:use-attribute-sets="normal.para.spacing"
   start-indent="0.2in" text-indent="-0.2in">

   <xsl:call-template name="inline.boldseq">
    <xsl:with-param name="content">
     <xsl:apply-templates select="citetitle" mode="bibliomixed.mode"/>
    </xsl:with-param>
   </xsl:call-template>

   <fo:block>
    <xsl:call-template name="bibliomixed-common"/>
   </fo:block>

  </fo:block>

 </xsl:template>

</xsl:stylesheet>	

<!-- 
Local variables:
mode: xml
sgml-indent-step: 1
sgml-indent-data: 1
End:
-->


