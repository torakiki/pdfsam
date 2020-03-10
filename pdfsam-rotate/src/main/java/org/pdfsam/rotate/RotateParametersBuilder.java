/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/giu/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.rotate;

import static java.util.Objects.isNull;

import java.sql.Array;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

import org.pdfsam.support.params.AbstractPdfOutputParametersBuilder;
import org.pdfsam.support.params.MultipleOutputTaskParametersBuilder;
import org.pdfsam.task.BulkRotateParameters;
import org.pdfsam.task.PdfRotationInput;
import org.sejda.common.collection.NullSafeSet;
import org.sejda.model.input.PdfSource;
import org.sejda.model.output.SingleOrMultipleTaskOutput;
import org.sejda.model.pdf.page.PageRange;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.sejda.model.rotation.Rotation;

/**
 * Builder for {@link BulkRotateParameters}
 * 
 * @author Andrea Vacondio
 *
 */
class RotateParametersBuilder extends AbstractPdfOutputParametersBuilder<BulkRotateParameters>
        implements MultipleOutputTaskParametersBuilder<BulkRotateParameters> {

    private SingleOrMultipleTaskOutput output;
    private String prefix;
    private Set<PdfRotationInput> inputs = new NullSafeSet<>();
    private Rotation rotation;
    private PredefinedSetOfPages predefinedRotationType;

    void addInput(PdfSource<?> source, Set<PageRange> pageSelection) {
        if (isNull(pageSelection) || pageSelection.isEmpty()) {
            this.inputs.add(new PdfRotationInput(source, rotation, predefinedRotationType));
        } else {
        	/*
        	 * START: Sanket M 8th March 2020 - PDFSam merge change request - #ps3
        	 * Create page Range array
        	 */
        	// OLD CODE: this.inputs.add(new PdfRotationInput(source, rotation, pageSelection.stream().toArray(PageRange[]::new)));
        	ArrayList<PageRange> tempPageArray1 = new ArrayList<PageRange>();
        	if(predefinedRotationType == predefinedRotationType.EVEN_PAGES) {
        		pageSelection.forEach(new Consumer<PageRange>() {
					
					@Override
					public void accept(PageRange t) {
						for(int i = t.getStart(); i <= t.getEnd(); i++) {
							if(i%2 == 0) {
								tempPageArray1.add(new PageRange(i,i));
							}
						}
					}
        		});
        		
        		this.inputs.add(new PdfRotationInput(source, rotation, tempPageArray1.toArray(PageRange[]::new)));
        	}
        	if(predefinedRotationType == predefinedRotationType.ODD_PAGES) {
        		pageSelection.forEach(new Consumer<PageRange>() {
					
					@Override
					public void accept(PageRange t) {
						for(int i = t.getStart(); i <= t.getEnd(); i++) {
							if(i%2 != 0) {
								tempPageArray1.add(new PageRange(i,i));
							}
						}
					}
        		});
        		
        		this.inputs.add(new PdfRotationInput(source, rotation, tempPageArray1.toArray(PageRange[]::new)));
        	}
        	if(predefinedRotationType == predefinedRotationType.ALL_PAGES) {
        		this.inputs.add(new PdfRotationInput(source, rotation, pageSelection.stream().toArray(PageRange[]::new)));
        	}
        	/*
        	 * END 
        	 */
        }
    }

    boolean hasInput() {
        return !inputs.isEmpty();
    }

    @Override
    public void output(SingleOrMultipleTaskOutput output) {
        this.output = output;
    }

    @Override
    public void prefix(String prefix) {
        this.prefix = prefix;
    }

    protected SingleOrMultipleTaskOutput getOutput() {
        return output;
    }

    protected String getPrefix() {
        return prefix;
    }

    public void rotation(Rotation rotation) {
        this.rotation = rotation;
    }

    public void rotationType(PredefinedSetOfPages predefinedRotationType) {
        this.predefinedRotationType = predefinedRotationType;

    }

    @Override
    public BulkRotateParameters build() {
        BulkRotateParameters params = new BulkRotateParameters();
        params.setCompress(isCompress());
        params.setExistingOutputPolicy(existingOutput());
        params.setVersion(getVersion());
        params.setOutput(getOutput());
        params.setOutputPrefix(getPrefix());
        inputs.forEach(params::addInput);
        return params;
    }

}
