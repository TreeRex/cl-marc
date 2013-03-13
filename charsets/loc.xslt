<!-- Extract the Han ideograph EACC / UCS mappings from the LoC MARC8 table -->

<xsl:stylesheet version="2.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text"/>

<xsl:template match="/">
  <!-- Grouping 9.1 is 'East Asian/CJK EACC/Ideographs' -->
  <xsl:apply-templates select="codeTables/codeTable/characterSet/grouping[@number='9.1']/code"/>
</xsl:template>

<xsl:template match="code">
  <xsl:value-of select="marc"/>
  <xsl:text>&#x9;</xsl:text>
  <!-- The LoC table contains several mappings to the PUA with alternates to
       GETA. We don't want to map into the PUA so use the alternates if available -->
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
