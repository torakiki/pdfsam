/* 
 * This file is part of the PDF Black project
 * Created on 6 feb 2021
 * Copyright 2021 by Sober Lemur S.a.s di Vacondio Andrea (info@soberlemur.com).
 *
 * You are not permitted to distribute it in any form unless explicit
 * consent is given by Sober Lemur S.a.s di Vacondio Andrea.
 * You are not permitted to modify it.
 *
 * PDF Black is distributed WITHOUT ANY WARRANTY; 
 * without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 */
/**
 * @author Andrea Vacondio
 *
 */
module org.pdfsam.persistence {
    exports org.pdfsam.persistence;

    requires transitive com.fasterxml.jackson.databind;
    requires java.prefs;
    requires org.sejda.commons;
    requires org.slf4j;
}