<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
	<xsl:template match="@XML.boldID">
		<xsl:element name="input">
			<xsl:attribute name="type">hidden</xsl:attribute>
			<xsl:attribute name="name">TBoldID</xsl:attribute>
			<xsl:attribute name="value"><xsl:value-of select="."/></xsl:attribute>
		</xsl:element>
		<xsl:apply-templates/>
	</xsl:template>
	<xsl:template match="Departments">
		<span style="font-weight:bold">Departments:   </span>
		<xsl:for-each select="../../Department[@XML.boldID=..//@XML.idref]">
			<br/>
			<xsl:for-each select="Name">
				<span style="cursor:hand">
					<xsl:element name="input">
						<xsl:attribute name="type">text</xsl:attribute>
						<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
						<xsl:attribute name="value"><xsl:apply-templates/></xsl:attribute>
					</xsl:element>
				</span>
				<xsl:apply-templates select="@XML.boldID"/>
			</xsl:for-each>
		</xsl:for-each>
	</xsl:template>
	<xsl:template match="address">
		<span style="font-weight:bold">Address:   </span>
		<xsl:for-each select="../../Address[@XML.boldID=..//@XML.idref]">
			<br/>
			<xsl:for-each select="street">
				<xsl:text>Street     </xsl:text> 
				<br/>
				<xsl:element name="input">
					<xsl:attribute name="type">text</xsl:attribute>
					<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
					<xsl:attribute name="value"><xsl:apply-templates/></xsl:attribute>
				</xsl:element>
			</xsl:for-each>
			<br/>
			<xsl:for-each select="postcode">
				<xsl:text>PostCode 	</xsl:text>
				<br/>
				<xsl:element name="input">
					<xsl:attribute name="type">text</xsl:attribute>
					<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
					<xsl:attribute name="value"><xsl:apply-templates/></xsl:attribute>
				</xsl:element>
			</xsl:for-each>			
			<br/>
			<xsl:for-each select="city">
				<xsl:text >City     </xsl:text> 
				<br/>
				<xsl:element name="input">
					<xsl:attribute name="type">text</xsl:attribute>
					<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
					<xsl:attribute name="value"><xsl:apply-templates/></xsl:attribute>
				</xsl:element>
			</xsl:for-each>
			<br/>
			<xsl:for-each select="country">
				<xsl:text>Country  </xsl:text>
				<br/>
				<xsl:element name="input">
					<xsl:attribute name="type">text</xsl:attribute>
					<xsl:attribute name="name"><xsl:value-of select="@XML.boldID"/></xsl:attribute>
					<xsl:attribute name="value"><xsl:apply-templates/></xsl:attribute>
				</xsl:element>
			</xsl:for-each>
			<br/>
		</xsl:for-each>
	</xsl:template>
	<xsl:template match="Company">
		<div>
			<xsl:for-each select="CompanyName">
				<span style="color:maroon; font-size:medium">
					<xsl:text>Company name </xsl:text>
					<br/>
					<xsl:element name="input">
						<xsl:attribute name="type">text</xsl:attribute>
						<xsl:attribute name="name"><xsl:value-of select="XML.boldID"/></xsl:attribute>
						<xsl:attribute name="value"><xsl:apply-templates/></xsl:attribute>						
					</xsl:element>
					<br/>
				</span>
			</xsl:for-each>
		</div>
		<xsl:for-each select="Offices">
			<div/>
			<xsl:for-each select="Office">
				<xsl:apply-templates select="."/>
			</xsl:for-each>
			<br/>
		</xsl:for-each>
		<br/>
	</xsl:template>
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
			<br/>
			<xsl:for-each select="Departments">
				<xsl:apply-templates select="."/>
			</xsl:for-each>
		</xsl:for-each>
	</xsl:template>
	<xsl:template match="/">
		<html>
			<head>
				<script language="javascript">
			function EditObject(BoldId)
			{
			}
			</script>
			</head>
			<body>
				<form method="Post" name="frmMain" action="">
					<br/>
					<xsl:element name="input">
						<xsl:attribute name="type">hidden</xsl:attribute>
						<xsl:attribute name="name">Action</xsl:attribute>
						<xsl:attribute name="value">Update</xsl:attribute>
					</xsl:element>							
					<xsl:for-each select="OrgChartClasses">
					<span style="font-size:large">Edit Company</span> 
					<br/>
					<br/>
						<xsl:for-each select="Company">
							<xsl:apply-templates select="."/>
							<xsl:element name="input">
								<xsl:attribute name="type">submit</xsl:attribute>
								<xsl:attribute name="value">Save</xsl:attribute>
								<xsl:attribute name="name">btnSubmit</xsl:attribute>
							</xsl:element>
						</xsl:for-each>
						<br/>
					</xsl:for-each>
				</form>
			</body>
		</html>
	</xsl:template>
</xsl:stylesheet>
