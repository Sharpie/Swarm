<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:html="http://www.w3.org/TR/REC-html40"
 version="1.0">

 <xsl:import href="http://docbook.sourceforge.net/release/xsl/1.50.0/html/docbook.xsl"/>
 
 <xsl:import href="fest2002-common.xsl"/>

 <!-- turn into "strong" HTML element -->
 <xsl:template match="emphasis[@role='strong']">
  <strong><xsl:apply-templates/></strong>
 </xsl:template>

 <!-- sort biblioentries in alpha order -->
 <xsl:template match="bibliography|bibliography/bibliodiv">
  <h2>
   <xsl:call-template name="anchor">
    <xsl:with-param name="node" select="."/>
   </xsl:call-template>
   <xsl:value-of select="title"/>
  </h2>
  <xsl:call-template name="sort-biblio-common"/>
 </xsl:template>        
 
 <xsl:template match="bibliomixed">
  <xsl:variable name="id">
    <xsl:call-template name="object.id"/>
  </xsl:variable>

  <!-- a divider -->
  <hr/>
  <!-- create the HTML anchor -->

  <div class="{name(.)}">
   <xsl:call-template name="anchor"/>
   <xsl:apply-templates select="citetitle" mode="bibliomixed.mode"/>
   <xsl:call-template name="bibliomixed-common"/>
   </div>
 </xsl:template>

 <xsl:template match="citetitle" mode="bibliomixed.mode">
  <strong><em><xsl:apply-templates mode="bibliomixed.mode"/></em></strong>
  <br/>
 </xsl:template>
 
 <xsl:template match="subtitle" mode="bibliomixed.mode">
  <em><xsl:apply-templates mode="bibliomixed.mode"/></em>
 </xsl:template>

 <xsl:template match="abstract/para[1]" mode="bibliomixed.mode">
  <p><strong>Abstract: </strong>
  <xsl:apply-templates mode="bibliomixed.mode"/></p>
 </xsl:template>

 <xsl:template match="remark" mode="bibliomixed.mode">
  <font color="red"><em><xsl:apply-templates/></em></font>
 </xsl:template>

 <xsl:template match="remark">
  <font color="red"><em><xsl:apply-templates/></em></font>
 </xsl:template>

 <xsl:template match="email">
  <xsl:call-template name="inline.monoseq">
   <xsl:with-param name="content">
    <xsl:text>&lt;</xsl:text>

    <!-- obfuscate e-mail address -->
    <xsl:value-of select="substring-before(.,'@')"/> 
    <xsl:text> at </xsl:text>
    <xsl:value-of select="substring-after(.,'@')"/> 

    <!--  <a>
    <xsl:attribute name="href">mailto:<xsl:value-of select="."/>
   </xsl:attribute>
    <xsl:apply-templates/>
   </a>
    -->
    
    <xsl:text>&gt;</xsl:text>
   </xsl:with-param>
  </xsl:call-template>
 </xsl:template>
 
</xsl:stylesheet>	

<!-- 
Local variables:
mode: xml
sgml-indent-step: 1
sgml-indent-data: 1
End:
-->


