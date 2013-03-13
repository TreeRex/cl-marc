<!-- Extract the ANSEL mappings from the LoC MARC-8 table -->

<xsl:stylesheet version="2.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text"/>

<xsl:template match="/">
  <!-- Grouping 9.1 is 'East Asian/CJK EACC/Ideographs' -->
  <xsl:apply-templates select="codeTables/codeTable/characterSet[@ISOcode='45']/code"/>
</xsl:template>

<xsl:template match="code">
  <xsl:value-of select="marc"/>
  <xsl:text>&#x9;</xsl:text>
  <xsl:choose>
    <xsl:when test="element(alt)">
      <xsl:value-of select="alt"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="ucs"/>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>&#xA;</xsl:text>
</xsl:template>

</xsl:stylesheet>
