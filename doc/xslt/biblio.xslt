<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
<page xmlns="http://projectmallard.org/1.0/"
xmlns:gloss="http://projectmallard.org/experimental/gloss/"
  id="biblio" type="gloss:glossary">
	<info>
    <xsl:for-each select="bibliography/biblioentry">
        <gloss:term>
            <xsl:attribute name="id">
                <xsl:value-of select="@id" />
            </xsl:attribute>

            <title><xsl:value-of select="@id"/></title>
            <p>
                <span style="authors">
                    <xsl:for-each select="authorgroup/author">
                        <xsl:if test="position() &gt; 1">
                            <xsl:choose>
                                <xsl:when test="position() = last()"><xsl:text> and </xsl:text></xsl:when>
                                <xsl:otherwise><xsl:text>, </xsl:text></xsl:otherwise>
                            </xsl:choose>
                        </xsl:if>
                        <xsl:value-of select="firstname"/><xsl:text> </xsl:text><xsl:value-of select="surname"/>
                    </xsl:for-each>
                </span>
                <xsl:text> </xsl:text>
                <span style="pubdate">(<xsl:value-of select="pubdate"/>)</span>
                <xsl:text>. </xsl:text>
                <span style="title"><xsl:value-of select="title"/></span>
                <xsl:text>. </xsl:text>
                <span style="publisher"><xsl:value-of select="publisher/publishername"/></span>
                <xsl:text>. </xsl:text>
                <span style="pages">
                    <xsl:value-of select="volumenum"/>
                    <xsl:text>:</xsl:text>
                    <xsl:value-of select="pagenums"/>
                    <xsl:text>.</xsl:text>
                </span>
                <!-- TODO: add DOI (and other biblioid) links -->
            </p>
        </gloss:term>
	</xsl:for-each>
	</info>
    <title><xsl:value-of select="bibliography/title"/></title>

</page>
</xsl:template>
</xsl:stylesheet>

