<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:html="http://www.w3.org/TR/REC-html40"
 version="1.0">

 <xsl:param name="generate.toc"></xsl:param>
 <xsl:param name="biblioentry.item.separator">, </xsl:param>

 <!-- don't generate brackets around xref text -->
 <xsl:template match="biblioentry|bibliomixed" mode="xref-to-prefix"/>
 <xsl:template match="biblioentry|bibliomixed" mode="xref-to-suffix"/>

 <!-- make xref text be the citetitle  -->
 <xsl:template match="biblioentry|bibliomixed" mode="xref-to">
  <!-- handles both biblioentry and bibliomixed -->
  <xsl:choose>
   <xsl:when test="string(.) = ''">
    <xsl:variable name="bib" select="document($bibliography.collection)"/>
    <xsl:variable name="id" select="@id"/>
    <xsl:variable name="entry" select="$bib/bibliography/*[@id=$id][1]"/>
    <xsl:choose>
     <xsl:when test="$entry">
      <xsl:choose>
       <xsl:when test="local-name($entry/*[1]) = 'abbrev'">
	<xsl:apply-templates select="$entry/*[1]"/>
       </xsl:when>
       <xsl:otherwise>
	<xsl:value-of select="@id"/>
       </xsl:otherwise>
      </xsl:choose>
     </xsl:when>
     <xsl:otherwise>
      <xsl:message>
       <xsl:text>No bibliography entry: </xsl:text>
       <xsl:value-of select="$id"/>
       <xsl:text> found in </xsl:text>
       <xsl:value-of select="$bibliography.collection"/>
      </xsl:message>
      <xsl:value-of select="@id"/>
     </xsl:otherwise>
    </xsl:choose>
   </xsl:when>
   <xsl:otherwise>
    <xsl:choose>
     <xsl:when test="local-name(*[1]) = 'abbrev'">
      <xsl:apply-templates select="*[1]"/>
     </xsl:when>
     <xsl:otherwise>

      <!-- cache nodesets for later use -->

      <xsl:variable name="theorgname"
       select="authorgroup/author[1]/affiliation/orgname|author/affiliation/orgname"/>
      <xsl:variable name="theaddress"
       select="authorgroup/author[1]/affiliation/address|author/affiliation/address"/>

      <!-- firstname -->
      <xsl:value-of select="authorgroup/author[1]/firstname|author/firstname"/>

      <!-- space -->
      <xsl:text> </xsl:text>

      <!-- surname -->
      <xsl:value-of
       select="authorgroup/author[1]/surname|author/surname"/>
      
      <xsl:choose>
       <!-- if author has an organization use that -->
       <xsl:when test="$theorgname!=''">
	<xsl:text> (</xsl:text>
	<xsl:value-of select="$theorgname"/>
	<xsl:text>)</xsl:text>
       </xsl:when>
       <!-- otherwise see if author has a city and country and use that -->
       <xsl:when test="$theaddress/city and $theaddress/country">
	<xsl:text> (</xsl:text>
	<xsl:value-of select="$theaddress/city"/><xsl:text>, </xsl:text>
	<xsl:value-of select="$theaddress/country"/><xsl:text>)</xsl:text>
       </xsl:when>
       <!-- otherwise list no affiliation -->
       <xsl:otherwise></xsl:otherwise>
      </xsl:choose>

      <!-- put double quotes around citetitle -->
      <xsl:text>, &#x201C;</xsl:text>
      <xsl:value-of select="citetitle"/>
      <xsl:text>&#x201D;</xsl:text>

     </xsl:otherwise>
    </xsl:choose>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:template>

 <xsl:template name="sort-biblio-common">
  <xsl:apply-templates select="*[not(self::bibliomixed)]"/>
  <xsl:apply-templates select="bibliomixed">
   <xsl:sort data-type="text" select="@id"/>
  </xsl:apply-templates>
 </xsl:template>

 <xsl:template name="bibliomixed-common">
   <!-- author and authorgroup -->
   <xsl:apply-templates select="author|authorgroup" mode="bibliomixed.mode"/>
   <!-- then everything else (calls common templates) -->
   <xsl:apply-templates select="*[not(self::citetitle|self::author|self::authorgroup)]" mode="bibliomixed.mode"/>
 </xsl:template>

 <xsl:template match="authorgroup" mode="bibliomixed.mode">
  <xsl:for-each select="author">
   <xsl:apply-templates select="." mode="bibliomixed.mode"/>
   <xsl:if test="position()!=last()"><xsl:text> and </xsl:text></xsl:if>
  </xsl:for-each>
 </xsl:template>

 <xsl:template match="author" mode="bibliomixed.mode">
  <xsl:call-template name="person.name"/>
  <xsl:text> </xsl:text>
  <xsl:apply-templates select="affiliation/address/email" mode="bibliography.mode"/>
  <xsl:text> (</xsl:text>
  <xsl:for-each select="affiliation">
   <xsl:apply-templates select="." mode="bibliomixed.mode"/>
   <xsl:if test="position()!=last()"><xsl:text> and </xsl:text></xsl:if>
  </xsl:for-each>
  <xsl:text>) </xsl:text>
 </xsl:template>

 <xsl:template match="affiliation" mode="bibliomixed.mode">
  <xsl:apply-templates select="orgdiv" mode="bibliography.mode"/>
  <xsl:apply-templates select="orgname" mode="bibliomixed.mode"/>
  <xsl:if test="(position()!=last() or address/city or address/country) and orgname">
   <xsl:value-of select="$biblioentry.item.separator"/>
  </xsl:if>
  <xsl:value-of select="address/city"/>
  <xsl:if test="address/city and address/country">
   <xsl:value-of select="$biblioentry.item.separator"/>
  </xsl:if>
  <xsl:value-of select="address/country"/>
 </xsl:template>

</xsl:stylesheet>	

<!-- 
Local variables:
mode: xml
sgml-indent-step: 1
sgml-indent-data: 1
End:
-->


