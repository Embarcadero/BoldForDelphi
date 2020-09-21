<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<xsl:template match="Office">
		<div/>
		<br/>
		<xsl:for-each select="../../../Office[@XML.boldID=..//@XML.idref]">
			<div>
				<xsl:for-each select="Name">
					<span style="font-size:medium">
					<xsl:text>Office Name </xsl:text>
					<br/>
					<xsl:element name="input">
						<xsl:attribute name="type">text</xsl:attribute>
						<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
						<xsl:attribute name="value"><xsl:apply-templates/></xsl:attribute>
					</xsl:element>
					</span>
				</xsl:for-each>
			</div>
			<xsl:for-each select="phone">
				<span style="font-weight:bold">Phone   </span>
				<br/>
				<xsl:element name="input">
					<xsl:attribute name="type">text</xsl:attribute>
					<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
					<xsl:attribute name="value"><xsl:apply-templates/></xsl:attribute>
				</xsl:element>
			</xsl:for-each>
			<br/>
			<xsl:for-each select="fax">
				<span style="font-weight:bold">Fax   </span>
				<br/>
				<xsl:element name="input">
					<xsl:attribute name="type">text</xsl:attribute>
					<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
					<xsl:attribute name="value"><xsl:apply-templates/></xsl:attribute>
				</xsl:element>
			</xsl:for-each>
			<br/>
			<xsl:for-each select="Email">
				<span style="font-weight:bold">Email   </span>
				<br/>
				<xsl:element name="input">
					<xsl:attribute name="type">text</xsl:attribute>
					<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
					<xsl:attribute name="value"><xsl:apply-templates/></xsl:attribute>
				</xsl:element>
			</xsl:for-each>
			<br/>
			<br/>
			<xsl:for-each select="address">
				<xsl:apply-templates select="."/>
			</xsl:for-each>
			<br/>
		</xsl:for-each>
	</xsl:template>

	<xsl:template match="/">
		<html>
			<head/>
			<body>
				<span style="font-size:large">Edit Department</span>
				<br/>
				<br/>
				<xsl:for-each select="OrgChartClasses">
					<form method="Post" name="frmMain" action="">	
						<xsl:element name="input">
							<xsl:attribute name="type">hidden</xsl:attribute>
							<xsl:attribute name="name">Action</xsl:attribute>
							<xsl:attribute name="value">Update</xsl:attribute>
						</xsl:element>													
						<xsl:for-each select="Department">
							<xsl:for-each select="Name">
								<span style="font-size:medium">						
									<xsl:text>Deparment Name:</xsl:text>
								</span>								
								<br/>
								<xsl:element name="input">
									<xsl:attribute name="type">text</xsl:attribute>
									<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
									<xsl:attribute name="size">30</xsl:attribute>
									<xsl:attribute name="value">						
										<xsl:apply-templates/>
									</xsl:attribute>
								</xsl:element>
							</xsl:for-each>
							<br/>
							<xsl:for-each select="Office">
								<xsl:for-each select="Office">
									<xsl:apply-templates select="."/>
								</xsl:for-each>
							</xsl:for-each>
						</xsl:for-each>
						<xsl:element name="input">
							<xsl:attribute name="type">submit</xsl:attribute>
							<xsl:attribute name="value">Save</xsl:attribute>
							<xsl:attribute name="name">btnSubmit</xsl:attribute>
						</xsl:element>						
					</form>
				</xsl:for-each>
			</body>
		</html>
	</xsl:template>
</xsl:stylesheet>
